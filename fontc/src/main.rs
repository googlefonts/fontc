use std::{io::{Write, BufWriter}, time::Instant, fs::OpenOptions};

use clap::Parser;

use fontbe::orchestration::Context as BeContext;
use fontc::{init_paths, write_font_file, Args, ChangeDetector, Config, Error};
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
    let t0 = Instant::now();
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

    let args = Args::parse();
    let (ir_paths, be_paths) = init_paths(&args)?;
    let config = Config::new(args)?;
    let prev_inputs = config.init()?;

    let mut change_detector = ChangeDetector::new(config.clone(), ir_paths.clone(), prev_inputs)?;
    let workload = fontc::create_workload(&mut change_detector, t0)?;

    let fe_root = FeContext::new_root(
        config.args.flags(),
        ir_paths,
        workload.current_inputs().clone(),
    );
    let be_root = BeContext::new_root(config.args.flags(), be_paths, &fe_root);
    let mut timing = workload.exec(&fe_root, &be_root)?;

    if config.args.flags().contains(Flags::EMIT_THREADS) {
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
