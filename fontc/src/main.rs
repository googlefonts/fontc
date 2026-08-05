use std::io::Write;

use clap::Parser;
use tracing::{error, warn};
use tracing_log::NormalizeEvent;
use tracing_subscriber::{
    EnvFilter,
    fmt::{FmtContext, FormatEvent, FormatFields, format::Writer},
    registry::LookupSpan,
};

mod args;

use args::Args;
use fontbe::orchestration::AnyWorkId;
use fontc::{Error, JobTimer};

struct LogFormatter;

impl<S, N> FormatEvent<S, N> for LogFormatter
where
    S: tracing::Subscriber + for<'a> LookupSpan<'a>,
    N: for<'a> FormatFields<'a> + 'static,
{
    fn format_event(
        &self,
        ctx: &FmtContext<'_, S, N>,
        mut writer: Writer<'_>,
        event: &tracing::Event<'_>,
    ) -> std::fmt::Result {
        let normalized_meta = event.normalized_metadata();
        let meta = normalized_meta.as_ref().unwrap_or_else(|| event.metadata());
        let ts = chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Micros, true);
        let thread_id = std::thread::current().id();
        let target = meta.target();
        let level = meta.level();

        if writer.has_ansi_escapes() {
            let (style_start, style_end) = match *level {
                tracing::Level::ERROR => ("\x1b[1;31m", "\x1b[0m"),
                tracing::Level::WARN => ("\x1b[33m", "\x1b[0m"),
                tracing::Level::INFO => ("\x1b[32m", "\x1b[0m"),
                tracing::Level::DEBUG => ("\x1b[34m", "\x1b[0m"),
                tracing::Level::TRACE => ("\x1b[36m", "\x1b[0m"),
            };
            write!(
                writer,
                "[{ts} {thread_id:?} {target} {style_start}{level}{style_end}] "
            )?;
        } else {
            write!(writer, "[{ts} {thread_id:?} {target} {level}] ")?;
        }

        ctx.format_fields(writer.by_ref(), event)?;
        writeln!(writer)
    }
}

fn main() {
    let args = Args::parse();

    // catch and print errors manually, to avoid just seeing the Debug impls
    // The default log level is warn so the user will see it unless they specifically turned off logging
    if let Err(e) = run(args) {
        let mut error_displayed = false;
        let mut additional = "";
        if let Error::Backend(fontbe::error::Error::FeaCompileError(e)) = &e {
            if tracing::enabled!(tracing::Level::WARN) {
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
    // default to WARN; RUST_LOG or --log can still override
    let filter = if let Some(log_filters) = &args.log {
        EnvFilter::builder()
            .with_default_directive(tracing::level_filters::LevelFilter::WARN.into())
            .parse_lossy(log_filters)
    } else {
        EnvFilter::builder()
            .with_default_directive(tracing::level_filters::LevelFilter::WARN.into())
            .from_env_lossy()
    };
    tracing_subscriber::fmt()
        .event_format(LogFormatter)
        .with_env_filter(filter)
        .init();
    timer.add(time.complete());

    let input = args.source()?;
    let options = args.try_into()?;
    fontc::run(input, options, timer)
}

fn print_verbose_version() -> Result<(), std::io::Error> {
    let version = fontc::version();
    // In a git-less build (e.g. `cargo install`) vergen's idempotent mode sets
    // VERGEN_GIT_SHA to the literal "VERGEN_IDEMPOTENT_OUTPUT" sentinel (see
    // fontbe::version); drop the "@ <sha>" rather than print that. is_empty()
    // covers a var that wasn't emitted at all.
    let sha = option_env!("VERGEN_GIT_SHA").unwrap_or_default();
    if sha.is_empty() || sha.starts_with("VERGEN_") {
        writeln!(std::io::stdout(), "{} {version}\n", env!("CARGO_PKG_NAME"))?;
    } else {
        writeln!(
            std::io::stdout(),
            "{} {version} @ {sha}\n",
            env!("CARGO_PKG_NAME"),
        )?;
    }
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
