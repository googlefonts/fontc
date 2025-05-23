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
    #[cfg(not(target_family = "wasm"))]
    t0: Instant,
    job_times: HashMap<ThreadId, Vec<JobTimeState>>,
}

impl Default for JobTimer {
    fn default() -> Self {
        Self {
            #[cfg(not(target_family = "wasm"))]
            t0: Instant::now(),
            job_times: Default::default(),
        }
    }
}

impl JobTimer {
    /// Prepare to time things using the provided time zero.
    pub fn new() -> Self {
        Default::default()
    }

    /// Start timing a job.
    ///
    /// Meant to be called when a job is runnable, that is it's ready to be
    /// submitted to an execution system such as a threadpool.
    ///
    /// nth_wave shows on mouseover in the final output. Each time we release
    /// a group of jobs - because their dependencies are complate - we increment
    /// it so one can see the graph progression in the timing svg output.
    pub fn create_timer(&self, id: AnyWorkId, nth_wave: usize) -> JobTime {
        #[cfg(not(target_family = "wasm"))]
        {
            JobTime::Runnable(JobTimeState::new(id, nth_wave))
        }
        #[cfg(target_family = "wasm")]
        {
            JobTime::Nop
        }
    }

    pub fn add(&mut self, timing: JobTime) {
        let state = match timing {
            JobTime::Done(state) => state,
            other => {
                log::warn!("trying to add unfinished timing '{other:?}'");
                return;
            }
        };
        self.job_times
            .entry(state.thread_id)
            .or_default()
            .push(state);
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
        AnyWorkId::Fe(FeWorkIdentifier::ColorPalettes) => "cpal",
        AnyWorkId::Fe(FeWorkIdentifier::Features) => "fea",
        AnyWorkId::Fe(FeWorkIdentifier::GlobalMetrics) => "metrics",
        AnyWorkId::Fe(FeWorkIdentifier::Glyph(..)) => "glyph",
        AnyWorkId::Fe(FeWorkIdentifier::GlyphOrder) => "glyphorder",
        AnyWorkId::Fe(FeWorkIdentifier::KerningGroups) => "kerngrps",
        AnyWorkId::Fe(FeWorkIdentifier::KernInstance(..)) => "kernat",
        AnyWorkId::Fe(FeWorkIdentifier::PaintGraph) => "colr",
        AnyWorkId::Fe(FeWorkIdentifier::PreliminaryGlyphOrder) => "pre-go",
        AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata) => "static-meta",
        AnyWorkId::Be(BeWorkIdentifier::Avar) => "avar",
        AnyWorkId::Be(BeWorkIdentifier::Cmap) => "cmap",
        AnyWorkId::Be(BeWorkIdentifier::Colr) => "colr-be",
        AnyWorkId::Be(BeWorkIdentifier::Cpal) => "cpal-be",
        AnyWorkId::Be(BeWorkIdentifier::Features) => "fea",
        AnyWorkId::Be(BeWorkIdentifier::FeaturesAst) => "fea.ast",
        AnyWorkId::Be(BeWorkIdentifier::Font) => "font",
        AnyWorkId::Be(BeWorkIdentifier::Fvar) => "fvar",
        AnyWorkId::Be(BeWorkIdentifier::Gasp) => "gasp",
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
        AnyWorkId::Be(BeWorkIdentifier::Meta) => "meta",
        AnyWorkId::Be(BeWorkIdentifier::Stat) => "STAT",
        AnyWorkId::Be(BeWorkIdentifier::Vhea) => "vhea",
        AnyWorkId::Be(BeWorkIdentifier::Vmtx) => "vmtx",
        AnyWorkId::Be(BeWorkIdentifier::ExtraFeaTables) => "ExtraFeaTables",
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

/// Inner state for timing tasks.
#[derive(Debug, Clone)]
#[cfg(not(target_family = "wasm"))]
pub struct JobTimeState {
    id: AnyWorkId,
    nth_wave: usize,
    thread_id: ThreadId,
    queued: Instant,
    run: Instant,
    complete: Instant,
}

#[cfg(not(target_family = "wasm"))]
impl JobTimeState {
    fn new(id: AnyWorkId, nth_wave: usize) -> Self {
        let now = Instant::now();
        JobTimeState {
            id,
            nth_wave,
            thread_id: std::thread::current().id(),
            queued: now,
            run: now,
            complete: now,
        }
    }
}

/// A state machine tracking timer state.
#[derive(Debug, Clone)]
#[cfg(not(target_family = "wasm"))]
pub enum JobTime {
    /// Initial state of a runnable job.
    Runnable(JobTimeState),
    /// State after submission to an execution queue.
    Queued(JobTimeState),
    /// State after the job starts running
    Running(JobTimeState),
    /// State when the job has finished.
    Done(JobTimeState),
}

#[cfg(target_family = "wasm")]
/// Timer on wasm does nothing
pub enum JobTime {
    #[derive(Debug, Clone)]
    Nop,
}

impl JobTime {
    /// Queue the timer. Expects the timer to be runnable.
    #[cfg(not(target_family = "wasm"))]
    pub fn queued(self) -> Self {
        match self {
            JobTime::Runnable(mut state) => {
                state.queued = Instant::now();
                JobTime::Queued(state)
            }
            other => {
                log::warn!("attempting to queue non-runnable timer: {other:?}");
                other
            }
        }
    }

    #[cfg(target_family = "wasm")]
    pub fn queued(self) -> Self {
        self
    }

    /// Run the work.
    ///
    /// Expects the timer to be runnable or queued.
    #[cfg(not(target_family = "wasm"))]
    pub fn run(self) -> Self {
        match self {
            // you can call 'run' without queuing, if needed
            JobTime::Runnable(mut state) | JobTime::Queued(mut state) => {
                state.run = Instant::now();
                JobTime::Running(state)
            }
            other => {
                log::warn!("attempting to rerun timer: {other:?}");
                other
            }
        }
    }

    #[cfg(target_family = "wasm")]
    pub fn run(self) -> Self {
        self
    }

    /// Finish the timer. Expects the timer to be running.
    #[cfg(not(target_family = "wasm"))]
    pub fn complete(self) -> Self {
        match self {
            JobTime::Running(mut s) => {
                s.complete = Instant::now();
                JobTime::Done(s)
            }
            other => {
                log::warn!("attempting to complete non-running timer: {other:?}");
                other
            }
        }
    }

    #[cfg(target_family = "wasm")]
    pub fn complete(self) -> Self {
        self
    }
}
