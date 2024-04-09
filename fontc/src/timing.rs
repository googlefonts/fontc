//! Helps to understand what threads in fontc are up to.
//!
//! <https://github.com/googlefonts/fontc/pull/443> discusses motivation.

use fontbe::orchestration::{AnyWorkId, WorkId as BeWorkIdentifier};
use fontir::orchestration::WorkId as FeWorkIdentifier;
use std::{collections::HashMap, io, thread::ThreadId, time::Instant};

/// Tracks time for jobs that run on many threads.
///
/// Meant for use with a threadpool. For example, build timing for each
/// unit of work submitted to something like rayon and accumulate them here.
///
/// Currently not threadsafe, meant to be used by a central orchestrator because
/// that happens to be what fontc does.
#[derive(Debug)]
pub struct JobTimer {
    /// The beginning of time
    t0: Instant,
    job_times: HashMap<ThreadId, Vec<JobTime>>,
}

impl JobTimer {
    /// Prepare to time things using the provided time zero.
    pub fn new(t0: Instant) -> Self {
        JobTimer {
            t0,
            job_times: Default::default(),
        }
    }

    pub fn add(&mut self, timing: JobTime) {
        self.job_times
            .entry(timing.thread_id)
            .or_default()
            .push(timing);
    }

    pub fn write_svg(&mut self, out: &mut impl io::Write) -> Result<(), io::Error> {
        let names: HashMap<_, _> = self
            .job_times
            .keys()
            .enumerate()
            .map(|(i, tid)| (*tid, format!("t{i}")))
            .collect();
        for timings in self.job_times.values_mut() {
            timings.sort_by_key(|t| (t.run - self.t0).as_nanos());
        }
        let mut names: Vec<_> = names.into_iter().map(|(k, v)| (v, k)).collect();
        names.sort_by(|(n1, _), (n2, _)| n1.cmp(n2));

        let end_time = self
            .job_times
            .values()
            .flat_map(|ts| ts.iter())
            .map(|t| t.complete - self.t0)
            .max()
            .unwrap_or_default();

        let prefix = r#"
            <svg xmlns="http://www.w3.org/2000/svg">
            <style type="text/css">
                text {
                font-family: monospace;
                font-size: 12pt;
                }
            </style>"#;

        writeln!(out, "{prefix}")?;
        for (i, (_, tid)) in names.iter().enumerate() {
            let timings = self.job_times.get(tid).unwrap();
            let line_height = 15;
            let text_height = 12;
            let box_top = line_height * i;
            let text_y = box_top + text_height;
            for timing in timings {
                let job_start = (timing.run - self.t0).as_secs_f64();
                let job_end = (timing.complete - self.t0).as_secs_f64();
                let job_queued = (timing.queued - self.t0).as_secs_f64();
                let begin_pct = 100.0 * job_start / end_time.as_secs_f64();
                let exec_pct =
                    100.0 * (timing.complete - timing.run).as_secs_f64() / end_time.as_secs_f64();
                let fill = color(&timing.id);
                // enables use of browser console, e.g. $('g[work='something'])
                writeln!(
                    out,
                    "  <g work=\"{}\">",
                    format!("{:?}", timing.id).replace('\"', "")
                )
                .unwrap();
                writeln!(
                    out,
                    "    <rect x=\"{begin_pct:.2}%\" y=\"{box_top}\" width=\"{exec_pct:.2}%\" height=\"{line_height}\"  fill=\"{fill}\" stroke=\"black\" />"
                )
                .unwrap();
                if fill == "gray" {
                    let text = short_name(&timing.id);
                    writeln!(
                        out,
                        "    <text x=\"{begin_pct:.2}%\" y=\"{text_y}\" width=\"{exec_pct:.2}%\" height=\"{text_height}\" >{text}</text>",
                    )
                    .unwrap();
                }
                writeln!(
                    out,
                    "<title>{:.0}ms ({:.2}%) {:?}\nqueued at {:.0}ms\nrun at {:.0}ms\ndone at {:.0}ms\nWave {}</title>",
                    1000.0 * (job_end - job_start),
                    exec_pct,
                    timing.id,
                    1000.0 * job_queued,
                    1000.0 * job_start,
                    1000.0 * job_end,
                    timing.nth_wave,
                )
                .unwrap();
                writeln!(out, "  </g>").unwrap();
            }
        }
        writeln!(out, "</svg>")
    }
}

fn short_name(id: &AnyWorkId) -> &'static str {
    match id {
        AnyWorkId::Fe(FeWorkIdentifier::Anchor(..)) => "anchor",
        AnyWorkId::Fe(FeWorkIdentifier::Features) => "fea",
        AnyWorkId::Fe(FeWorkIdentifier::GlobalMetrics) => "metrics",
        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..)) => "glyph",
        AnyWorkId::Fe(FeWorkIdentifier::GlyphIrDelete(..)) => "rm ir",
        AnyWorkId::Fe(FeWorkIdentifier::GlyphOrder) => "glyphorder",
        AnyWorkId::Fe(FeWorkIdentifier::KerningGroups) => "kerngrps",
        AnyWorkId::Fe(FeWorkIdentifier::KernInstance(..)) => "kernat",
        AnyWorkId::Fe(FeWorkIdentifier::PreliminaryGlyphOrder) => "pre-go",
        AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata) => "static-meta",
        AnyWorkId::Be(BeWorkIdentifier::Avar) => "avar",
        AnyWorkId::Be(BeWorkIdentifier::Cmap) => "cmap",
        AnyWorkId::Be(BeWorkIdentifier::Features) => "fea",
        AnyWorkId::Be(BeWorkIdentifier::FeaturesAst) => "fea.ast",
        AnyWorkId::Be(BeWorkIdentifier::Font) => "font",
        AnyWorkId::Be(BeWorkIdentifier::Fvar) => "fvar",
        AnyWorkId::Be(BeWorkIdentifier::Gdef) => "GDEF",
        AnyWorkId::Be(BeWorkIdentifier::Glyf) => "glyf",
        AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(..)) => "glyf-frag",
        AnyWorkId::Be(BeWorkIdentifier::Gpos) => "GPOS",
        AnyWorkId::Be(BeWorkIdentifier::Gsub) => "GSUB",
        AnyWorkId::Be(BeWorkIdentifier::Gvar) => "gvar",
        AnyWorkId::Be(BeWorkIdentifier::GvarFragment(..)) => "gvar-frag",
        AnyWorkId::Be(BeWorkIdentifier::Head) => "head",
        AnyWorkId::Be(BeWorkIdentifier::Hhea) => "hhea",
        AnyWorkId::Be(BeWorkIdentifier::Hmtx) => "hmtx",
        AnyWorkId::Be(BeWorkIdentifier::Hvar) => "HVAR",
        AnyWorkId::Be(BeWorkIdentifier::GatherIrKerning) => "kern-be",
        AnyWorkId::Be(BeWorkIdentifier::KernFragment(..)) => "kern-frag",
        AnyWorkId::Be(BeWorkIdentifier::GatherBeKerning) => "kern-gather-be",
        AnyWorkId::Be(BeWorkIdentifier::Loca) => "loca",
        AnyWorkId::Be(BeWorkIdentifier::LocaFormat) => "loca-fmt",
        AnyWorkId::Be(BeWorkIdentifier::Marks) => "Marks",
        AnyWorkId::Be(BeWorkIdentifier::Maxp) => "maxp",
        AnyWorkId::Be(BeWorkIdentifier::Mvar) => "MVAR",
        AnyWorkId::Be(BeWorkIdentifier::Name) => "name",
        AnyWorkId::Be(BeWorkIdentifier::Os2) => "OS/2",
        AnyWorkId::Be(BeWorkIdentifier::Post) => "post",
        AnyWorkId::Be(BeWorkIdentifier::Stat) => "STAT",
        AnyWorkId::InternalTiming(name) => name,
    }
}

fn color(id: &AnyWorkId) -> &'static str {
    match id {
        AnyWorkId::Fe(FeWorkIdentifier::Features) => "#e18707",
        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..)) => "#830356",
        AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(..)) => "#00c1c9",
        AnyWorkId::Be(BeWorkIdentifier::GvarFragment(..)) => "#008786",
        AnyWorkId::InternalTiming(..) => "#009a00",
        _ => "gray",
    }
}

/// Start timing a job.
///
/// Meant to be called when a job is runnable, that is it's ready to be
/// submitted to an execution system such as a threadpool.
///
/// nth_wave shows on mouseover in the final output. Each time we release
/// a group of jobs - because their dependencies are complate - we increment
/// it so one can see the graph progression in the timing svg output.
pub fn create_timer(id: AnyWorkId, nth_wave: usize) -> JobTimeRunnable {
    JobTimeRunnable {
        id,
        nth_wave,
        runnable: Instant::now(),
    }
}

/// The initial state of a runnable job.
///
/// It may have queued from t0 to launchable but now it's go-time!
pub struct JobTimeRunnable {
    id: AnyWorkId,
    nth_wave: usize,
    runnable: Instant,
}

impl JobTimeRunnable {
    /// Time of submission to execution queue, e.g. thread pool submission
    pub fn queued(self) -> JobTimeQueued {
        JobTimeQueued {
            id: self.id,
            nth_wave: self.nth_wave,
            runnable: self.runnable,
            queued: Instant::now(),
        }
    }
}

pub struct JobTimeQueued {
    id: AnyWorkId,
    nth_wave: usize,
    runnable: Instant,
    queued: Instant,
}

impl JobTimeQueued {
    /// Time job actually starts running, e.g. beginning of thread pool execution
    ///
    /// Jobs are presumed to not hop threads; we capture the current thread so we
    /// can aggregate the time of jobs on each runner thread.
    pub fn run(self) -> JobTimeRunning {
        JobTimeRunning {
            id: self.id,
            nth_wave: self.nth_wave,
            thread: std::thread::current().id(),
            runnable: self.runnable,
            queued: self.queued,
            run: Instant::now(),
        }
    }
}

pub struct JobTimeRunning {
    id: AnyWorkId,
    nth_wave: usize,
    thread: ThreadId,
    runnable: Instant,
    queued: Instant,
    run: Instant,
}

impl JobTimeRunning {
    /// Jobs done, we have all the significant time slices.
    ///
    /// Gives us the final timing to submit to [JobTimer]
    pub fn complete(self) -> JobTime {
        JobTime {
            id: self.id,
            nth_wave: self.nth_wave,
            thread_id: self.thread,
            _runnable: self.runnable,
            queued: self.queued,
            run: self.run,
            complete: Instant::now(),
        }
    }
}

/// Times are relative to t0 in a [JobTimer]
#[derive(Debug, Clone)]
pub struct JobTime {
    id: AnyWorkId,
    nth_wave: usize,
    thread_id: ThreadId,
    _runnable: Instant,
    queued: Instant,
    run: Instant,
    pub(crate) complete: Instant,
}

impl JobTime {
    /// A JobTime for something that took no time, a nop
    pub fn nop(id: AnyWorkId) -> Self {
        let now = Instant::now();
        JobTime {
            id,
            nth_wave: 0,
            thread_id: std::thread::current().id(),
            _runnable: now,
            queued: now,
            run: now,
            complete: now,
        }
    }
}
