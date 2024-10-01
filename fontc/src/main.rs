use std::{io::Write, time::Instant};

use clap::Parser;

use fontbe::orchestration::AnyWorkId;
use fontc::{create_timer, Args, Error, JobTimer};
use log::{error, warn};

fn main() {
    let args = Args::parse();

    // catch and print errors manually, to avoid just seeing the Debug impls
    if let Err(e) = run(args) {
        let mut error_displayed = false;
        let mut additional = "";
        if let Error::Backend(fontbe::error::Error::FeaCompileError(e)) = &e {
            if log::log_enabled!(log::Level::Warn) {
                warn!("{}", e.display_verbose());
                error_displayed = true;
            } else {
                additional =
                    ", set log level to warn or higher (export RUST_LOG=warn) for additional detail"
            }
        }
        if !error_displayed {
            error!("{e}{additional}");
        }
        std::process::exit(1);
    }
}

fn run(args: Args) -> Result<(), Error> {
    // handle `--vv` verbose version argument request
    if args.verbose_version {
        print_verbose_version().map_err(Error::StdioWriteFail)?;
        std::process::exit(0);
    }
    let mut timer = JobTimer::new(Instant::now());
    let time = create_timer(AnyWorkId::InternalTiming("Init logger"), 0)
        .queued()
        .run();
    env_logger::builder()
        .format(|buf, record| {
            let ts = buf.timestamp_micros();
            let style = buf.default_level_style(record.level());
            writeln!(
                buf,
                "[{ts} {:?} {} {style}{}{style:#}] {}",
                std::thread::current().id(),
                record.target(),
                record.level(),
                record.args()
            )
        })
        .init();
    timer.add(time.complete());

    fontc::run(args, timer)
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
