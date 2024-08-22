//! generating html reports from crater results

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Display,
    ops::Sub,
    path::Path,
};

use crate::{
    error::Error,
    ttx_diff_runner::{CompilerFailure, DiffError, DiffOutput},
};
use maud::{html, Markup};

use super::{DiffResults, RunSummary};

static HTML_FILE: &str = "index.html";

pub(super) fn generate(target_dir: &Path) -> Result<(), Error> {
    let summary_path = target_dir.join(super::SUMMARY_FILE);
    let summary: Vec<RunSummary> = crate::try_read_json(&summary_path)?;
    let details = summary
        .iter()
        .flat_map(|run| match run.try_load_results(target_dir) {
            Some(Ok(results)) => Some(Ok((run.fontc_rev.clone(), results))),
            None => None,
            Some(Err(e)) => Some(Err(e)),
        })
        .collect::<Result<HashMap<_, _>, _>>()?;

    let html_text = make_html(&summary, &details);
    let outpath = target_dir.join(HTML_FILE);
    crate::try_write_str(&html_text, &outpath)
}

fn make_html(summary: &[RunSummary], results: &HashMap<String, DiffResults>) -> String {
    let table_body = make_table_body(summary);
    let css = include_str!("../../resources/style.css");
    let table = html! {
        table #results {
            thead {
                tr {
                    th.date scope="col" { "date" }
                    th.rev scope="col" { "rev" }
                    th.total scope="col" { "targets" }
                    th.identical scope="col" { "identical" }
                    th.fontc_err scope="col" { "fontc 💥" }
                    th.fontmake_err scope="col" { "fontmake 💥" }
                    th.both_err scope="col" { "both 💥" }
                    th.other_err scope="col" { "other 💥" }
                    th.diff_erc scope="col" { "similarity %" }
                }
            }
            (table_body)
        }
    };
    let detailed_report = match summary {
        [.., prev, current] => make_detailed_report(
            results.get(&current.fontc_rev).unwrap(),
            results.get(&prev.fontc_rev).unwrap(),
        ),
        _ => html!(),
    };

    html! {
        (maud::DOCTYPE)
        html {
            head {
                title { "fontc_crater results" }
                style { (css) }
                meta charset="utf-8";
            }
            body {
                h1 { "fontc_crater" }
                div #explain {
                    "compiling a large number of target fonts with both fontc and fontmake, \
                        comparing the results.
                        "
                }
                (table)
                (detailed_report)
            }
        }
    }
    .into_string()
}

fn make_table_body(runs: &[RunSummary]) -> Markup {
    // make the '+32' or '-1.114' elements that annotate the results
    fn make_cmp<T: PartialOrd + Copy + Sub<Output = T> + Display>(
        current: T,
        prev: Option<T>,
        more_is_better: bool,
    ) -> Markup {
        let Some(prev) = prev else {
            return Default::default();
        };

        let diff = current - prev;
        let diff = format!("{diff:+.3}");
        match current.partial_cmp(&prev) {
            Some(std::cmp::Ordering::Equal) | None => Default::default(),
            Some(std::cmp::Ordering::Greater) if more_is_better => html! { span.better { (diff) } },
            Some(std::cmp::Ordering::Less) if !more_is_better => html! { span.better { (diff) } },
            Some(_) => html! { span.worse { (diff) } },
        }
    }
    fn make_row(run: &RunSummary, prev: Option<&RunSummary>) -> Markup {
        let total_diff = make_cmp(
            run.stats.total_targets as i32,
            prev.map(|p| p.stats.total_targets as i32),
            true,
        );
        let identical_diff = make_cmp(
            run.stats.identical as i32,
            prev.map(|p| p.stats.identical as i32),
            true,
        );
        let fontc_err_diff = make_cmp(
            run.stats.fontc_failed as i32,
            prev.map(|p| p.stats.fontc_failed as i32),
            false,
        );
        let fontmake_err_diff = make_cmp(
            run.stats.fontmake_failed as i32,
            prev.map(|p| p.stats.fontmake_failed as i32),
            false,
        );
        let both_err_diff = make_cmp(
            run.stats.both_failed as i32,
            prev.map(|p| p.stats.both_failed as i32),
            false,
        );
        let other_err_diff = make_cmp(
            run.stats.other_failure as i32,
            prev.map(|p| p.stats.other_failure as i32),
            false,
        );
        let diff_perc_diff = make_cmp(
            run.stats.diff_perc_including_failures,
            prev.map(|p| p.stats.diff_perc_including_failures),
            true,
        );
        let diff_fmt = format!("{:.3}", run.stats.diff_perc_including_failures);
        html! {
            tr.run {
                td.date { (run.began.format("%Y-%m-%d %H:%M:%S")) }
                td.rev { ( run.fontc_rev.get(..16).unwrap_or(run.fontc_rev.as_str()) ) }
                td.total {  ( run.stats.total_targets) " " (total_diff) }
                td.identical {  (run.stats.identical) " " (identical_diff)  }
                td.fontc_err {  (run.stats.fontc_failed) " " (fontc_err_diff)  }
                td.fontmake_err {  (run.stats.fontmake_failed) " " (fontmake_err_diff)  }
                td.both_err {  (run.stats.both_failed) " " (both_err_diff) }
                td.other_err {  (run.stats.other_failure) " " (other_err_diff)  }
                td.diff_perc {  (diff_fmt) " " (diff_perc_diff) }
            }
        }
    }

    let iter = runs.iter().enumerate().map(|(i, run)| {
        let prev = (i > 0).then(|| &runs[i - 1]);
        make_row(run, prev)
    });

    html! {
        tbody {
            @for run in iter {
                (run)
            }
        }
    }
}

fn make_detailed_report(current: &DiffResults, prev: &DiffResults) -> Markup {
    let error_report = make_error_report(current, prev);
    let diff_report = make_diff_report(current, prev);
    html! {
        (diff_report)
        (error_report)
    }
}

fn make_diff_report(current: &DiffResults, prev: &DiffResults) -> Markup {
    fn get_total_diff_ratios(results: &DiffResults) -> BTreeMap<&Path, f32> {
        results
            .success
            .iter()
            .map(|(k, v)| {
                (
                    k.as_path(),
                    match v {
                        DiffOutput::Identical => 100.0,
                        DiffOutput::Diffs(d) => d.get("total").unwrap().ratio().unwrap() * 100.0,
                    },
                )
            })
            .collect()
    }

    let current_diff = get_total_diff_ratios(current);
    let prev_diff = get_total_diff_ratios(prev);
    let mut current_diff = current_diff.into_iter().collect::<Vec<_>>();
    current_diff.sort_by_key(|(_, r)| -(r * 1e6) as i64);

    let mut items = Vec::new();
    for (path, ratio) in &current_diff {
        let prev_ratio = prev_diff.get(path).copied();
        let delta = prev_ratio.map(|pr| *ratio - pr);
        let decoration = match delta {
            None => html!( span.better { (format!("{ratio:+.3}")) }),
            Some(0.0) => html!("—"),
            Some(d) if d.is_sign_positive() => html! ( span.better { (format!("{d:+.3}")) } ),
            Some(d) => html! ( span.worse { (format!("{d:+.3}")) } ),
        };
        items.push(html! {
            div.diff_group_item {
                div.font_path { (path.display()) }
                div.diff_result { (format!("{ratio:.3}%")) " " (decoration) }
            }
        })
    }

    html! {
        div.diff_report {
            h3 { "results" }
            @for item in items {
                (item)
            }
        }
    }
}

fn make_error_report(current: &DiffResults, prev: &DiffResults) -> Markup {
    let current_fontc = get_compiler_failures(current, "fontc");
    let prev_fontc = get_compiler_failures(prev, "fontc");
    let current_fontmake = get_compiler_failures(current, "fontmake");
    let prev_fontmake = get_compiler_failures(prev, "fontmake");

    let current_both = current_fontc
        .keys()
        .copied()
        .filter(|k| current_fontmake.contains_key(k))
        .collect::<HashSet<_>>();
    let prev_both = prev_fontc
        .keys()
        .copied()
        .filter(|k| prev_fontmake.contains_key(k))
        .collect::<HashSet<_>>();

    let current_other = get_other_failures(current);
    let prev_other = get_other_failures(prev);

    let fontc = (current_fontc.len() - current_both.len() > 0)
        .then(|| {
            make_error_report_group(
                "fontc",
                current_fontc
                    .keys()
                    .copied()
                    .filter(|k| !current_both.contains(k))
                    .map(|k| (k, !prev_fontc.contains_key(k))),
                |_| html!(),
            )
        })
        .unwrap_or_default();

    let fontmake = (current_fontmake.len() - current_both.len() > 0)
        .then(|| {
            make_error_report_group(
                "fontmake",
                current_fontmake
                    .keys()
                    .copied()
                    .filter(|k| (!current_both.contains(k)))
                    .map(|k| (k, !prev_fontmake.contains_key(k))),
                |_| html!(),
            )
        })
        .unwrap_or_default();
    let both = (!current_both.is_empty())
        .then(|| {
            make_error_report_group(
                "both",
                current_both
                    .iter()
                    .copied()
                    .map(|k| (k, !prev_both.contains(k))),
                |_| html!(),
            )
        })
        .unwrap_or_default();

    let other = (!current_other.is_empty())
        .then(|| {
            make_error_report_group(
                "other",
                current_other
                    .keys()
                    .copied()
                    .map(|k| (k, !prev_other.contains_key(k))),
                |_| html!(),
            )
        })
        .unwrap_or_default();

    html! {
        (fontc)
        (fontmake)
        (both)
        (other)
    }
}

fn make_error_report_group<'a>(
    group_name: &str,
    paths_and_if_is_new_error: impl Iterator<Item = (&'a Path, bool)>,
    _details: impl Fn(&Path) -> Markup,
) -> Markup {
    let items = make_error_report_group_items(paths_and_if_is_new_error, _details);

    html! {
        div.error_report {
            h3 { (group_name) " failures" }
            div.failures {
                (items)
            }
        }
    }
}

fn make_error_report_group_items<'a>(
    paths_and_if_is_new_error: impl Iterator<Item = (&'a Path, bool)>,
    _details: impl Fn(&Path) -> Markup,
) -> Markup {
    html! {
        @for (path, is_new) in paths_and_if_is_new_error {
            div.report_group_item { (path.display()) @if is_new { " 🆕" } }
        }
    }
}
fn get_other_failures(results: &DiffResults) -> BTreeMap<&Path, &str> {
    results
        .failure
        .iter()
        .filter_map(|(path, r)| match r {
            DiffError::CompileFailed(_) => None,
            DiffError::Other(err) => Some((path.as_path(), err.as_str())),
        })
        .collect()
}

fn get_compiler_failures<'a>(
    results: &'a DiffResults,
    compiler: &str,
) -> BTreeMap<&'a Path, &'a CompilerFailure> {
    let get_err = |err: &'a DiffError| {
        let DiffError::CompileFailed(compfail) = err else {
            return None;
        };
        match compiler {
            "fontc" => compfail.fontc.as_ref(),
            "fontmake" => compfail.fontmake.as_ref(),
            _ => panic!("this is quite unexpected"),
        }
    };

    results
        .failure
        .iter()
        .flat_map(|(path, r)| get_err(r).map(|e| (path.as_path(), e)))
        .collect()
}
