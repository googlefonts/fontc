use std::io::Write;

use clap::Parser;
use tracing::{error, warn};
use tracing_chrome::FlushGuard;
use tracing_log::NormalizeEvent;
use tracing_subscriber::{
    EnvFilter, Layer,
    fmt::{FmtContext, FormatEvent, FormatFields, format::Writer},
    layer::{Filter, SubscriberExt},
    registry::LookupSpan,
    util::SubscriberInitExt,
};

mod args;

use args::Args;
use fontc::Error;

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

    let _logging_guard = init_logging(&args);

    let input = args.source()?;
    let options = args.try_into()?;
    fontc::run(input, options)
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

#[must_use]
fn init_logging(args: &Args) -> Option<FlushGuard> {
    let fmt_filter = create_env_filter(args.log.as_deref());
    let (chrome_layer, flush_guard) = if let Some(trace_path) = args.trace_path() {
        if let Some(parent) = trace_path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let (layer, guard) = tracing_chrome::ChromeLayerBuilder::new()
            .file(trace_path)
            .include_args(true)
            .build();
        let chrome_filter = ChromeFilter(fmt_filter.clone());
        (Some(layer.with_filter(chrome_filter)), Some(guard))
    } else {
        (None, None)
    };
    let fmt_layer = tracing_subscriber::fmt::layer()
        .event_format(LogFormatter)
        .with_filter(fmt_filter);

    tracing_subscriber::registry()
        .with(fmt_layer)
        .with(chrome_layer)
        .init();
    flush_guard
}

fn create_env_filter(log_filters: Option<&str>) -> EnvFilter {
    // default to WARN; RUST_LOG or --log can still override
    if let Some(log_filters) = log_filters {
        EnvFilter::builder()
            .with_default_directive(tracing::level_filters::LevelFilter::WARN.into())
            .parse_lossy(log_filters)
    } else {
        EnvFilter::builder()
            .with_default_directive(tracing::level_filters::LevelFilter::WARN.into())
            .from_env_lossy()
    }
}

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

struct ChromeFilter(EnvFilter);

impl<S: tracing::Subscriber> Filter<S> for ChromeFilter {
    fn enabled(
        &self,
        meta: &tracing::Metadata<'_>,
        cx: &tracing_subscriber::layer::Context<'_, S>,
    ) -> bool {
        meta.is_span() || self.0.enabled(meta, cx.clone())
    }

    fn max_level_hint(&self) -> Option<tracing::level_filters::LevelFilter> {
        None
    }
}
