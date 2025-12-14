//! Helps to understand what threads in fontc are up to.
//!
//! <https://github.com/googlefonts/fontc/pull/443> discusses motivation.

use fontbe::orchestration::{AnyWorkId, WorkId as BeWorkIdentifier};
use fontir::orchestration::WorkId as FeWorkIdentifier;
use std::time::Duration;
use std::{collections::HashMap, thread::ThreadId};

#[cfg(target_family = "wasm")]
use dummy_instant::Instant;
#[cfg(not(target_family = "wasm"))]
use std::time::Instant;

/// WASM does not have access to the system time, so we use a dummy
/// implementation of `Instant` and `Duration` that does nothing.
#[cfg(target_family = "wasm")]
mod dummy_instant {
    use std::ops::Sub;

    /// A noop Instant for wasm
    #[derive(Clone, Default, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
    pub struct Instant;

    /// A noop Duration for wasm
    #[derive(Clone, Default, Copy, Debug, PartialEq, PartialOrd, Eq, Ord)]
    pub struct Duration;

    impl Instant {
        pub fn now() -> Self {
            Self
        }
    }

    impl Duration {
        pub fn as_secs_f64(self) -> f64 {
            0.0
        }

        pub fn as_nanos(self) -> u128 {
            0
        }
    }

    impl Sub for Instant {
        type Output = Duration;

        fn sub(self, _rhs: Self) -> Self::Output {
            Duration
        }
    }
}

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
    job_times: HashMap<ThreadId, Vec<JobTimeState>>,
}

impl Default for JobTimer {
    fn default() -> Self {
        Self {
            t0: Instant::now(),
            job_times: Default::default(),
        }
    }
}

struct SvgThread<'a> {
    timings: &'a [JobTimeState],
    active_duration: Duration,
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
        let now = Instant::now();
        let state = JobTimeState {
            id,
            nth_wave,
            thread_id: std::thread::current().id(),
            queued: now,
            run: now,
            complete: now,
        };
        JobTime::Ready(state)
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

    #[cfg(feature = "cli")]
    fn make_svg_thread(&mut self) -> Vec<SvgThread<'_>> {
        let mut threads: Vec<_> = self
            .job_times
            .values_mut()
            .map(|timings| {
                timings.sort_by_key(|t| (t.run - self.t0).as_nanos());
                let active_duration = timings
                    .iter()
                    .map(|t| t.complete.duration_since(t.run))
                    .sum();
                SvgThread {
                    timings,
                    active_duration,
                }
            })
            .collect();
        threads.sort_by_key(|t| std::cmp::Reverse(t.active_duration));
        threads
    }

    /// Writes an SVG timeline visualization of job execution across threads.
    ///
    /// Each row represents a thread, each rectangle a job. Hover for timing details.
    #[cfg(feature = "cli")]
    pub fn write_svg(&mut self, out: &mut impl std::io::Write) -> Result<(), std::io::Error> {
        let start = self.t0;
        let threads = self.make_svg_thread();
        let end_time = threads
            .iter()
            .flat_map(|t| t.timings.iter())
            .map(|t| t.complete - start)
            .max()
            .unwrap_or_default();

        let line_height = 15;
        let num_threads = threads.len();
        let margin_top = 20;
        let margin_bottom = 40;
        let margin_left = 80; // Increased to make room for thread labels
        let margin_right = 20;
        let graph_width = 1000;
        let axis_height = 30;
        let graph_height = line_height * num_threads + axis_height;

        let prefix = format!(
            r#"
            <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 {total_width} {total_height}">
            <style type="text/css">
                text {{
                font-family: monospace;
                font-size: 12pt;
                }}
            </style>"#,
            total_width = graph_width + margin_left + margin_right,
            total_height = graph_height + margin_top + margin_bottom,
        );

        writeln!(out, "{prefix}")?;

        // Draw x-axis
        let axis_y = margin_top + line_height * num_threads;
        writeln!(
            out,
            "  <line x1=\"{margin_left}\" y1=\"{axis_y}\" x2=\"{}\" y2=\"{axis_y}\" stroke=\"black\" />",
            margin_left + graph_width
        )?;
        // Add time markers and grid lines
        let end_ms = end_time.as_secs_f64() * 1000.0;
        let num_markers = 10;
        for i in 0..=num_markers {
            let pct = i as f64 / num_markers as f64;
            let time_ms = pct * end_ms;
            let x = margin_left + (pct * graph_width as f64) as i32;
            // Draw vertical grid line from top to axis
            writeln!(
                out,
                "  <line x1=\"{x}\" y1=\"{margin_top}\" x2=\"{x}\" y2=\"{axis_y}\" stroke=\"lightgray\" stroke-width=\"1\" />"
            )?;
            // Draw axis tick mark
            writeln!(
                out,
                "  <line x1=\"{x}\" y1=\"{axis_y}\" x2=\"{x}\" y2=\"{}\" stroke=\"black\" />",
                axis_y + 5
            )?;
            // Draw time label
            writeln!(
                out,
                "  <text x=\"{x}\" y=\"{}\" font-size=\"10\" text-anchor=\"middle\">{:.0}ms</text>",
                axis_y + 20,
                time_ms
            )?;
        }

        for (i, thread) in threads.iter().enumerate() {
            let text_height = 12;
            let box_top = margin_top + line_height * i;
            let text_y = box_top + text_height;

            // Draw thread ID label
            let num_jobs = thread.timings.len();

            writeln!(
                out,
                "  <text x=\"{}\" y=\"{text_y}\" font-size=\"10\" text-anchor=\"end\">",
                margin_left - 10
            )?;
            writeln!(out, "    <title>Thread {i}\n{num_jobs} jobs</title>")?;
            writeln!(out, "    t{i}")?;
            writeln!(out, "  </text>")?;

            for timing in thread.timings {
                let job_start = (timing.run - start).as_secs_f64();
                let job_end = (timing.complete - start).as_secs_f64();
                let job_queued = (timing.queued - start).as_secs_f64();
                let begin_pct = job_start / end_time.as_secs_f64();
                let exec_pct =
                    (timing.complete - timing.run).as_secs_f64() / end_time.as_secs_f64();
                let x = margin_left + (begin_pct * graph_width as f64) as i32;
                let width = (exec_pct * graph_width as f64) as i32;
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
                    "    <rect x=\"{x}\" y=\"{box_top}\" width=\"{width}\" height=\"{line_height}\"  fill=\"{fill}\" stroke=\"black\" />"
                )
                .unwrap();
                if fill == "gray" {
                    let text = short_name(&timing.id);
                    writeln!(
                        out,
                        "    <text x=\"{x}\" y=\"{text_y}\" width=\"{width}\" height=\"{text_height}\" >{text}</text>",
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
        AnyWorkId::Be(BeWorkIdentifier::Vvar) => "VVAR",
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
pub struct JobTimeState {
    id: AnyWorkId,
    nth_wave: usize,
    thread_id: ThreadId,
    queued: Instant,
    run: Instant,
    complete: Instant,
}

/// A state machine tracking timer progress.
#[derive(Debug, Clone)]
pub enum JobTime {
    /// The job that is ready to run.
    Ready(JobTimeState),
    /// The job has been submitted to a work queue.
    Queued(JobTimeState),
    /// The job is running.
    Running(JobTimeState),
    /// The job has completed.
    Done(JobTimeState),
}

impl JobTime {
    /// Record that the work has been queued (submitted to a threadpool)
    pub fn queued(self) -> Self {
        match self {
            JobTime::Ready(mut state) => {
                state.queued = Instant::now();
                JobTime::Queued(state)
            }
            other => {
                log::warn!("attempting to queue non-runnable timer: {other:?}");
                other
            }
        }
    }

    /// Mark that the work is running.
    ///
    /// This records the current time and the current thread Id.
    pub fn run(self) -> Self {
        match self {
            // you can call 'run' without queuing, if needed
            JobTime::Ready(mut state) | JobTime::Queued(mut state) => {
                state.run = Instant::now();
                state.thread_id = std::thread::current().id();
                JobTime::Running(state)
            }
            other => {
                log::warn!("attempting to rerun timer: {other:?}");
                other
            }
        }
    }

    /// Finish the timer. Expects the timer to be running.
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
}
