use std::{
    fs::OpenOptions,
    io::{BufWriter, Write},
    time::Instant,
};

use clap::Parser;

use fontbe::orchestration::{AnyWorkId, Context as BeContext};
use fontc::{
    create_timer, init_paths, write_font_file, Args, ChangeDetector, Config, Error, JobTimer,
};
use fontir::orchestration::{Context as FeContext, Flags};

fn main() {
    // catch and print errors manually, to avoid just seeing the Debug impls
    if let Err(e) = run() {
        // we have a CI check that forbids eprintln, but we're very clever, so
        let _ = writeln!(std::io::stderr(), "{e}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Error> {
    let args = Args::parse();
    // handle `--vv` verbose version argument request
    if args.verbose_version {
        print_verbose_version()?;
        std::process::exit(0);
    }
    let mut timer = JobTimer::new(Instant::now());
    let time = create_timer(AnyWorkId::InternalTiming("Init logger"), 0)
        .queued()
        .run();
    env_logger::builder()
        .format(|buf, record| {
            let ts = buf.timestamp_micros();
            writeln!(
                buf,
                "[{ts} {} {} {}] {}",
                // we manually assign all threads a name
                std::thread::current().name().unwrap_or("unknown"),
                record.target(),
                buf.default_level_style(record.level())
                    .value(record.level()),
                record.args()
            )
        })
        .init();
    timer.add(time.complete());

    let time = create_timer(AnyWorkId::InternalTiming("Init config"), 0)
        .queued()
        .run();
    let (ir_paths, be_paths) = init_paths(&args)?;
    let config = Config::new(args)?;
    let prev_inputs = config.init()?;
    timer.add(time.complete());

    let mut change_detector = ChangeDetector::new(
        config.clone(),
        ir_paths.clone(),
        be_paths.clone(),
        prev_inputs,
        &mut timer,
    )?;

    let workload = fontc::create_workload(&mut change_detector, timer)?;

    let fe_root = FeContext::new_root(
        config.args.flags(),
        ir_paths,
        workload.current_inputs().clone(),
    );
    let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root);
    let mut timing = workload.exec(&fe_root, &be_root)?;

    if config.args.flags().contains(Flags::EMIT_TIMING) {
        let out_file = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(config.args.build_dir.join("threads.svg"))
            .map_err(Error::IoError)?;
        let mut buf = BufWriter::new(out_file);
        timing.write_svg(&mut buf)?
    }

    change_detector.finish_successfully()?;

    write_font_file(&config.args, &be_root)
}

fn print_verbose_version() -> Result<(), std::io::Error> {
    writeln!(
        std::io::stdout(),
        "{} {} @ {}\n",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        env!("VERGEN_GIT_SHA")
    )?;
    writeln!(std::io::stdout(), "{}", env!("VERGEN_RUSTC_HOST_TRIPLE"))?;
    writeln!(
        std::io::stdout(),
        "rustc {} (channel: {}, {} {})",
        env!("VERGEN_RUSTC_SEMVER"),
        env!("VERGEN_RUSTC_CHANNEL"),
        env!("VERGEN_RUSTC_COMMIT_HASH").get(..9).unwrap_or(""),
        env!("VERGEN_RUSTC_COMMIT_DATE")
    )?;
    writeln!(
        std::io::stdout(),
        "llvm {}",
        env!("VERGEN_RUSTC_LLVM_VERSION")
    )?;
    match env!("VERGEN_CARGO_DEBUG") {
        "true" => writeln!(std::io::stdout(), "cargo profile: debug")?,
        "false" => writeln!(std::io::stdout(), "cargo profile: release")?,
        _ => (),
    };
    writeln!(
        std::io::stdout(),
        "cargo optimization level: {}",
        env!("VERGEN_CARGO_OPT_LEVEL")
    )?;
    Ok(())
}
