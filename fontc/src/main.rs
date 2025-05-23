use std::io::Write;

use clap::Parser;

use fontbe::orchestration::AnyWorkId;
use fontc::{Args, Error, JobTimer};
use log::{error, warn};

fn main() {
    let args = Args::parse();

    // catch and print errors manually, to avoid just seeing the Debug impls
    // The default log level is error so the user will see it unless they specifically turned off logging
    if let Err(e) = run(args) {
        let mut error_displayed = false;
        let mut additional = "";
        if let Error::Backend(fontbe::error::Error::FeaCompileError(e)) = &e {
            if log::log_enabled!(log::Level::Warn) {
                error!("{e}");
                if let Some(diagnostic) = e.diagnostics() {
                    warn!("{}", diagnostic.display());
                }
                error_displayed = true;
            } else {
                additional = ", set log level to warn or higher (--log warn) for additional detail"
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
    let mut timer = JobTimer::new();
    let time = timer
        .create_timer(AnyWorkId::InternalTiming("Init logger"), 0)
        .queued()
        .run();
    let mut log_cfg = env_logger::builder();
    log_cfg.format(|buf, record| {
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
    });
    if let Some(log_filters) = &args.log {
        log_cfg.parse_filters(log_filters);
    }
    log_cfg.init();
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
