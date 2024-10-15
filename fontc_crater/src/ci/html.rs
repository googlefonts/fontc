//! generating html reports from crater results

use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::Display,
    ops::Sub,
    path::{Path, PathBuf},
};

use crate::{
    error::Error,
    ttx_diff_runner::{CompilerFailure, DiffError, DiffOutput, DiffValue},
};
use maud::{html, Markup};

use super::{DiffResults, RunSummary};

static HTML_FILE: &str = "index.html";

pub(super) fn generate(target_dir: &Path) -> Result<(), Error> {
    let summary_path = target_dir.join(super::SUMMARY_FILE);
    let summary: Vec<RunSummary> = crate::try_read_json(&summary_path)?;
    let sources_path = target_dir.join(super::SOURCES_FILE);
    let sources: BTreeMap<PathBuf, String> =
        super::load_json_if_exists_else_default(&sources_path)?;
    let details = summary
        .iter()
        .flat_map(|run| match run.try_load_results(target_dir) {
            Some(Ok(results)) => Some(Ok((run.fontc_rev.clone(), results))),
            None => None,
            Some(Err(e)) => Some(Err(e)),
        })
        .collect::<Result<HashMap<_, _>, _>>()?;

    let html_text = make_html(&summary, &sources, &details)?;
    let outpath = target_dir.join(HTML_FILE);
    crate::try_write_str(&html_text, &outpath)
}

fn make_html(
    summary: &[RunSummary],
    sources: &BTreeMap<PathBuf, String>,
    results: &HashMap<String, DiffResults>,
) -> Result<String, Error> {
    let table_body = make_table_body(summary);
    let css = include_str!("../../resources/style.css");
    let table = html! {
        table #results {
            thead  {
                tr #results_head {
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
            sources,
        ),
        _ => html!(),
    };

    let raw_html = html! {
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
    .into_string();
    tidy_html(&raw_html)
}

fn tidy_html(raw_html: &str) -> Result<String, Error> {
    let opts = tidier::FormatOptions {
        // indent with tabs to reduce file size.
        // the '4' option is suggested by the docs; does not mean 4 tabs each indent?
        indent: tidier::Indent {
            size: 4,
            tabs: true,
            ..Default::default()
        },
        ..Default::default()
    };
    tidier::format(raw_html, false, &opts).map_err(Into::into)
}

fn make_table_body(runs: &[RunSummary]) -> Markup {
    fn make_row(run: &RunSummary, prev: Option<&RunSummary>, is_most_recent: bool) -> Markup {
        let total_diff = make_delta_decoration(
            run.stats.total_targets as i32,
            prev.map(|p| p.stats.total_targets as i32),
            More::IsBetter,
        );
        let identical_diff = make_delta_decoration(
            run.stats.identical as i32,
            prev.map(|p| p.stats.identical as i32),
            More::IsBetter,
        );
        let fontc_err_diff = make_delta_decoration(
            run.stats.fontc_failed as i32,
            prev.map(|p| p.stats.fontc_failed as i32),
            More::IsWorse,
        );
        let fontmake_err_diff = make_delta_decoration(
            run.stats.fontmake_failed as i32,
            prev.map(|p| p.stats.fontmake_failed as i32),
            More::IsWorse,
        );
        let both_err_diff = make_delta_decoration(
            run.stats.both_failed as i32,
            prev.map(|p| p.stats.both_failed as i32),
            More::IsWorse,
        );
        let other_err_diff = make_delta_decoration(
            run.stats.other_failure as i32,
            prev.map(|p| p.stats.other_failure as i32),
            More::IsWorse,
        );
        let diff_perc_diff = make_delta_decoration(
            run.stats.diff_perc_including_failures,
            prev.map(|p| p.stats.diff_perc_including_failures),
            More::IsBetter,
        );
        let diff_fmt = format!("{:.3}", run.stats.diff_perc_including_failures);
        let diff_url = format!(
            "https://github.com/googlefonts/fontc/compare/{}...{}/",
            prev.as_ref()
                .map(|p| p.fontc_rev.as_str())
                .unwrap_or(run.fontc_rev.as_str()),
            run.fontc_rev,
        );
        let short_rev = run.fontc_rev.get(..16).unwrap_or(run.fontc_rev.as_str());
        let err_cells = if is_most_recent {
            html! {
                td.fontc_err { a href = "#fontc-failures" {  (run.stats.fontc_failed) " " (fontc_err_diff)  } }
                td.fontmake_err { a href = "#fontmake-failures" {  (run.stats.fontmake_failed) " " (fontmake_err_diff)  } }
                td.both_err { a href = "#both-failures" {  (run.stats.both_failed) " " (both_err_diff) } }
                td.other_err { a href = "#other-failures" {  (run.stats.other_failure) " " (other_err_diff)  } }
            }
        } else {
            html! {
            td.fontc_err {  (run.stats.fontc_failed) " " (fontc_err_diff)  }
            td.fontmake_err {  (run.stats.fontmake_failed) " " (fontmake_err_diff)  }
            td.both_err {  (run.stats.both_failed) " " (both_err_diff) }
            td.other_err {  (run.stats.other_failure) " " (other_err_diff)  }
            }
        };
        html! {
            tr.run {
                td.date { (run.began.format("%Y-%m-%d %H:%M:%S")) }
                td.rev { a href=(diff_url) { (short_rev) } }
                td.total {  ( run.stats.total_targets) " " (total_diff) }
                td.identical {  (run.stats.identical) " " (identical_diff)  }
                (err_cells)

                td.diff_perc {  (diff_fmt) " " (diff_perc_diff) }
            }
        }
    }

    let iter = runs.iter().enumerate().map(|(i, run)| {
        let prev = (i > 0).then(|| &runs[i - 1]);
        let is_last = i == runs.len() - 1;
        make_row(run, prev, is_last)
    });

    html! {
        tbody {
            @for run in iter {
                (run)
            }
        }
    }
}

enum More {
    IsBetter,
    IsWorse,
}

impl More {
    fn is_better(&self) -> bool {
        matches!(self, More::IsBetter)
    }
}

/// for values that change between runs, make the little decoration span that says like,
/// "+0.333" or "-5"
fn make_delta_decoration<T: PartialOrd + Copy + Sub<Output = T> + Display + Default>(
    current: T,
    prev: Option<T>,
    more: More,
) -> Markup {
    let delta = prev.map(|prev| current - prev).unwrap_or(current);
    let diff = format!("{delta:+.3}");
    match prev.and_then(|prev| current.partial_cmp(&prev)) {
        Some(std::cmp::Ordering::Equal) => Default::default(),
        // don't write +0
        None if current == T::default() => Default::default(),
        None | Some(std::cmp::Ordering::Greater) if more.is_better() => {
            html! { span.better { (diff) } }
        }
        Some(std::cmp::Ordering::Less) if !more.is_better() => html! { span.better { (diff) } },
        Some(_) | None => html! { span.worse { (diff) } },
    }
}

fn make_detailed_report(
    current: &DiffResults,
    prev: &DiffResults,
    sources: &BTreeMap<PathBuf, String>,
) -> Markup {
    let error_report = make_error_report(current, prev, sources);
    let diff_report = make_diff_report(current, prev, sources);
    html! {
        (diff_report)
        (error_report)
    }
}

/// make the list of fonts that were both compiled successfully.
fn make_diff_report(
    current: &DiffResults,
    prev: &DiffResults,
    sources: &BTreeMap<PathBuf, String>,
) -> Markup {
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

    let get_repo_url = |path: &Path| sources.get(path).map(String::as_str).unwrap_or("#");

    let current_diff = get_total_diff_ratios(current);
    let prev_diff = get_total_diff_ratios(prev);
    let mut current_diff = current_diff.into_iter().collect::<Vec<_>>();
    current_diff.sort_by_key(|(_, r)| -(r * 1e6) as i64);

    let mut items = Vec::new();
    for (path, ratio) in &current_diff {
        let diff_details = current.success.get(*path).unwrap();
        let prev_details = prev.success.get(*path);
        let prev_ratio = prev_diff.get(path).copied();
        // don't include 100% matches in results unless they are new
        if *ratio == 100.0 && prev_ratio == Some(100.0) {
            continue;
        }

        let decoration = make_delta_decoration(*ratio, prev_ratio, More::IsBetter);
        let details = format_diff_report_details(diff_details, prev_details);
        // avoid .9995 printing as 100%
        let ratio_fmt = format!("{:.3}%", (ratio * 1000.0).floor() / 1000.0);

        items.push(html! {
            details {
                summary {
                    span.font_path {
                        a href = ({ get_repo_url(path) }) { (path.display()) }
                    }
                    span.diff_result { (ratio_fmt) " " (decoration) }
                }
                (details)
            }
        });
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

fn make_error_report(
    current: &DiffResults,
    prev: &DiffResults,
    sources: &BTreeMap<PathBuf, String>,
) -> Markup {
    let current_fontc = get_compiler_failures(current, "fontc");
    let prev_fontc = get_compiler_failures(prev, "fontc");
    let current_fontmake = get_compiler_failures(current, "fontmake");
    let prev_fontmake = get_compiler_failures(prev, "fontmake");

    let current_both = current_fontc
        .keys()
        .copied()
        .filter(|k| current_fontmake.contains_key(k))
        .collect::<BTreeSet<_>>();
    let prev_both = prev_fontc
        .keys()
        .copied()
        .filter(|k| prev_fontmake.contains_key(k))
        .collect::<BTreeSet<_>>();

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
                |path| {
                    current_fontc
                        .get(path)
                        .copied()
                        .map(format_compiler_error)
                        .unwrap_or_default()
                },
                sources,
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
                |path| {
                    current_fontmake
                        .get(path)
                        .copied()
                        .map(format_compiler_error)
                        .unwrap_or_default()
                },
                sources,
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
                |path| {
                    let fontc_err = current_fontc
                        .get(path)
                        .copied()
                        .map(format_compiler_error)
                        .unwrap_or_default();
                    let fontmake_err = current_fontmake
                        .get(path)
                        .copied()
                        .map(format_compiler_error)
                        .unwrap_or_default();

                    html! {
                        .h5 { "fontc" }
                        (fontc_err)
                        .h5 { "fontmake" }
                        (fontmake_err)
                    }
                },
                sources,
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
                |path| {
                    let msg = current_other.get(path).copied().unwrap_or_default();
                    html! {
                        div.backtrace {
                            (msg)
                        }
                    }
                },
                sources,
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

fn format_compiler_error(err: &CompilerFailure) -> Markup {
    html! {
            div.backtrace {
            div.stderr { (err.stderr) }

        }
    }
}

/// for a given diff, the detailed information on per-table changes
fn format_diff_report_details(current: &DiffOutput, prev: Option<&DiffOutput>) -> Markup {
    let value_decoration = |table, value: DiffValue| -> Markup {
        let prev_value = match prev {
            None => None,
            Some(DiffOutput::Identical) => Some(DiffValue::Ratio(1.0)),
            Some(DiffOutput::Diffs(tables)) => tables.get(table).cloned(),
        };

        match (value, prev_value) {
            (DiffValue::Ratio(r), None) | (DiffValue::Ratio(r), Some(DiffValue::Only(_))) => {
                make_delta_decoration(r * 100.0, None, More::IsBetter)
            }
            (DiffValue::Ratio(r), Some(DiffValue::Ratio(p))) => {
                make_delta_decoration(r * 100.0, Some(p * 100.0), More::IsBetter)
            }

            (DiffValue::Only(_), Some(DiffValue::Ratio(p))) => {
                make_delta_decoration(0.0, Some(p * 100.), More::IsBetter)
            }
            // if only one compiler is writing this and previously none were...
            // that seems weird/unlikely and I don't see a useful thing to report
            (DiffValue::Only(_), _) => Default::default(),
        }
    };

    // if we are identical, we use this empty set as a placeholder so the types
    // work below.
    let empty = BTreeMap::new();

    let diffs = match current {
        DiffOutput::Identical => &empty,
        DiffOutput::Diffs(diffs) => diffs,
    };

    // the current report doesn't include tables that match, but if something
    // didn't match in the prev report but matches now we still want to include it
    let all_changed_tables = current
        .iter_tables()
        .chain(prev.into_iter().flat_map(|x| x.iter_tables()))
        .collect::<BTreeSet<_>>();

    let all_items = all_changed_tables
        .into_iter()
        .map(|table| {
            let value = diffs.get(table).cloned().unwrap_or(DiffValue::Ratio(1.0));
            (table, value)
        })
        .collect::<Vec<_>>();

    html! {
        table.diff_info {
            thead {
                tr {
                    th.diff_table scope = "col" { "table" }
                    th.diff_value scope = "col" { "value" }
                }
            }
            @for (table, value) in all_items {
                tr.table_diff_row {
                    td.table_diff_name { (table) }
                    td.table_diff_value { (value) " " ( {value_decoration(table, value) }) }
                }
            }
        }
    }
}

fn make_error_report_group<'a>(
    group_name: &str,
    paths_and_if_is_new_error: impl Iterator<Item = (&'a Path, bool)>,
    details: impl Fn(&Path) -> Markup,
    sources: &BTreeMap<PathBuf, String>,
) -> Markup {
    let items = make_error_report_group_items(paths_and_if_is_new_error, details, sources);

    let elem_id = format!("{group_name}-failures");
    html! {
        div.error_report {
            h3 id=(elem_id) { (group_name) " failures" }
            div.failures {
                (items)
            }
        }
    }
}

fn make_error_report_group_items<'a>(
    paths_and_if_is_new_error: impl Iterator<Item = (&'a Path, bool)>,
    details: impl Fn(&Path) -> Markup,
    sources: &BTreeMap<PathBuf, String>,
) -> Markup {
    let get_repo_url = |path: &Path| sources.get(path).map(String::as_str).unwrap_or("#");
    html! {
            @for (path, is_new) in paths_and_if_is_new_error {
                details.report_group_item {
                    summary {
                        a href = ({ get_repo_url(path) }) { (path.display()) }
                         @if is_new { " 🆕" }
                }
                    (details(path))
            }
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
