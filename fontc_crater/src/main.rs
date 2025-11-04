//! run bulk operations on fonts

use std::{
    collections::BTreeMap,
    fmt::Display,
    io::Write,
    path::Path,
    process::{Command, Stdio},
    sync::atomic::{AtomicUsize, Ordering},
    time::{Duration, Instant},
};

use clap::Parser;
use rayon::{ThreadPoolBuilder, prelude::*};

mod args;
mod ci;
mod error;
mod target;
mod ttx_diff_runner;

use serde::{Serialize, de::DeserializeOwned};

use args::{Args, Commands};
use error::Error;
use target::{BuildType, Target};

fn main() {
    env_logger::init();
    let args = Args::parse();
    if let Err(e) = run(&args) {
        eprintln!("{e}");
    }
}

fn run(args: &Args) -> Result<(), Error> {
    match &args.command {
        Commands::Ci(args) => ci::run_ci(args),
    }
}

/// Results of all runs
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct Results<T, E> {
    pub(crate) success: BTreeMap<Target, T>,
    pub(crate) failure: BTreeMap<Target, E>,
}

/// The output of trying to run on one font.
///
/// We don't use a normal Result because failure is okay, we will report it all at the end.
enum RunResult<T, E> {
    Success(T),
    Fail(E),
}

#[allow(clippy::type_complexity)] // come on, it's not _that_ bad
fn run_all<T: Send, E: Send, Cx: Sync>(
    targets: Vec<Target>,
    context: &Cx,
    runner: impl Fn(&Cx, &Target) -> RunResult<T, E> + Send + Sync,
) -> Result<Vec<(Target, RunResult<T, E>)>, Error> {
    let total_targets = targets.len();
    let counter = AtomicUsize::new(0);
    let currently_running = AtomicUsize::new(0);
    let threadpool = ThreadPoolBuilder::new().build().unwrap();

    let results = threadpool.install(|| {
        targets
            .into_par_iter()
            .map(|target| {
                let i = counter.fetch_add(1, Ordering::Relaxed) + 1;
                currently_running.fetch_add(1, Ordering::Relaxed);
                log::debug!("starting {target} ({i}/{total_targets})");
                let start_t = Instant::now();
                let r = runner(context, &target);
                let total_t = start_t.elapsed();
                let n_running = currently_running.fetch_sub(1, Ordering::Relaxed);
                log::debug!(
                    "finished {target} in {} ({n_running} active)",
                    human_readable_duration(total_t)
                );
                (target, r)
            })
            .collect()
    });
    Ok(results)
}

/// Get the short sha of the current commit in the provided repository.
///
/// If no repo provided, run in current directory
///
/// returns `None` if the `git` command fails (for instance if the path is not
/// a git repository)
fn get_git_rev(repo_path: Option<&Path>) -> Option<String> {
    let mut cmd = std::process::Command::new("git");
    cmd.args(["rev-parse", "--short", "HEAD"]);

    if let Some(dir) = repo_path {
        cmd.current_dir(dir);
    }
    let output = cmd.output().unwrap();

    Some(
        std::str::from_utf8(&output.stdout)
            .expect("rev is always ascii/hex string")
            .trim()
            .to_owned(),
    )
}

fn pip_freeze_sha() -> String {
    let pipfreeze = Command::new("pip")
        .arg("freeze")
        .output()
        .expect("pip should be installed")
        .stdout;
    let pipfreeze = String::from_utf8(pipfreeze).expect("pip freeze output is utf-8");
    compute_sha1sum_skipping_ttx_diff(pipfreeze)
}

/// input is expected to be the raw output of `pip freeze`
fn compute_sha1sum_skipping_ttx_diff(pip_freeze_output: String) -> String {
    let mut sha1sum = Command::new("shasum")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("shasum should be preinstalled everywhere");

    let mut stdin = sha1sum.stdin.take().expect("Failed to open stdin");
    std::thread::spawn(move || {
        for line in pip_freeze_output.lines() {
            // we don't want to include ttx_diff in our pip requirements, we handle it separately.
            if !line.contains("ttx_diff") {
                stdin
                    .write_all(line.as_bytes())
                    .expect("failed to write to stdin");
            }
        }
    });

    let output = sha1sum.wait_with_output().expect("Failed to read stdout");
    assert!(output.status.success());

    std::str::from_utf8(output.stdout.trim_ascii())
        .expect("shasum output always ascii")
        .to_owned()
}

fn get_input_sha(path: &Path) -> String {
    let output = Command::new("shasum")
        .arg(path)
        .output()
        .expect("shasum should be installed everywhere");
    std::str::from_utf8(&output.stdout)
        .expect("shasum output always ascii")
        .trim()
        .to_owned()
}

impl<T, E> Results<T, E> {
    fn targets(&self) -> impl Iterator<Item = &Target> {
        self.success.keys().chain(self.failure.keys())
    }
}

impl<T, E> FromIterator<(Target, RunResult<T, E>)> for Results<T, E> {
    fn from_iter<I: IntoIterator<Item = (Target, RunResult<T, E>)>>(iter: I) -> Self {
        let mut out = Results::default();
        for (path, reason) in iter.into_iter() {
            match reason {
                RunResult::Success(output) => {
                    out.success.insert(path, output);
                }
                RunResult::Fail(reason) => {
                    out.failure.insert(path, reason);
                }
            }
        }
        out
    }
}

impl<T, E> Default for Results<T, E> {
    fn default() -> Self {
        Self {
            success: Default::default(),
            failure: Default::default(),
        }
    }
}

fn try_read_string(path: &Path) -> Result<String, Error> {
    std::fs::read_to_string(path).map_err(|error| Error::ReadFile {
        path: path.to_owned(),
        error,
    })
}

fn try_read_json<T: DeserializeOwned>(path: impl AsRef<Path>) -> Result<T, Error> {
    let path = path.as_ref();
    try_read_string(path).and_then(|content| {
        serde_json::from_str(&content).map_err(|error| Error::ParseJson {
            path: path.to_owned(),
            error,
        })
    })
}

fn try_write_str(s: &str, path: &Path) -> Result<(), Error> {
    std::fs::write(path, s).map_err(|error| Error::WriteFile {
        path: path.to_owned(),
        error,
    })
}

fn try_write_json<T: Serialize>(obj: &T, path: &Path) -> Result<(), Error> {
    serde_json::to_string_pretty(&obj)
        .map_err(|error| Error::WriteJson {
            path: path.to_owned(),
            error,
        })
        .and_then(|json_str| try_write_str(&json_str, path))
}

fn try_create_dir(path: &Path) -> Result<(), Error> {
    std::fs::create_dir_all(path).map_err(|error| Error::CreateDir {
        path: path.to_owned(),
        error,
    })
}

const SEC_PER_MIN: u64 = 60;
const MIN_PER_HOUR: u64 = 60;
const SEC_PER_HOUR: u64 = SEC_PER_MIN * MIN_PER_HOUR;

fn human_readable_duration(duration: Duration) -> impl Display {
    struct TimePrinter(Duration);
    impl Display for TimePrinter {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let hours = self.0.as_secs() / SEC_PER_HOUR;
            let minutes = (self.0.as_secs() % SEC_PER_HOUR) / SEC_PER_MIN;
            let seconds =
                self.0.as_secs_f64() - ((hours * MIN_PER_HOUR + minutes) * SEC_PER_MIN) as f64;
            if hours > 0 {
                write!(f, "{hours}h")?;
            }
            write!(f, "{minutes}m")?;
            write!(f, "{seconds:.2}s")
        }
    }

    TimePrinter(duration)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn time_print_smoke_test() {
        let some_hours = Duration::from_secs(SEC_PER_HOUR * 2 + SEC_PER_MIN * 42 + 11);
        assert_eq!(
            human_readable_duration(some_hours).to_string(),
            "2h42m11.00s"
        );

        let mins2secs11point3355 = Duration::from_secs_f32(131.3355);
        assert_eq!(
            human_readable_duration(mins2secs11point3355).to_string(),
            "2m11.34s"
        );
    }

    #[test]
    fn pip_freeze_sha_skips_ttx_diff() {
        let one = "strictyaml==1.7.3
tabulate==0.9.0
-e git+ssh://git@github.com/googlefonts/fontc.git@0a4fa039a#egg=ttx_diff&subdirectory=ttx_diff
typing_extensions==4.15.0
youseedee==0.7.0
zopfli==0.2.3.post1
";

        // same as one but ttx_diff has a different sha
        let two = "strictyaml==1.7.3
tabulate==0.9.0
-e git+ssh://git@github.com/googlefonts/fontc.git@deadbeefc0w#egg=ttx_diff&subdirectory=ttx_diff
typing_extensions==4.15.0
youseedee==0.7.0
zopfli==0.2.3.post1
";
        // same as two but zopfli has a different version
        let three = "strictyaml==1.7.3
tabulate==0.9.0
-e git+ssh://git@github.com/googlefonts/fontc.git@deadbeefc0w#egg=ttx_diff&subdirectory=ttx_diff
typing_extensions==4.15.0
youseedee==0.7.0
zopfli==0.9.3.post1
";

        assert_eq!(
            compute_sha1sum_skipping_ttx_diff(one.into()),
            compute_sha1sum_skipping_ttx_diff(two.into())
        );
        assert_ne!(
            compute_sha1sum_skipping_ttx_diff(two.into()),
            compute_sha1sum_skipping_ttx_diff(three.into())
        );
    }
}
