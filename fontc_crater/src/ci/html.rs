//! generating html reports from crater results

use std::{collections::HashMap, fmt::Display, ops::Sub, path::Path};

use crate::error::Error;
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

fn make_html(summary: &[RunSummary], _results: &HashMap<String, DiffResults>) -> String {
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
