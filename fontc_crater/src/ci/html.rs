//! generating html reports from crater results

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    ffi::OsStr,
    fmt::Display,
    ops::Sub,
    path::{Path, PathBuf},
};

use crate::{
    BuildType, Target, TargetId,
    error::Error,
    ttx_diff_runner::{CompileFailed, CompilerFailure, DiffError, DiffOutput, DiffValue},
};
use maud::{Markup, PreEscaped, html};

use super::{
    DiffResults, RunSummary,
    accepted::{AcceptedFailure, AcceptedFailures, FailingCompiler, Surprise},
};

static HTML_FILE: &str = "index.html";
static ANNOTATIONS_FILE: &str = "annotations.json";

/// An annotation describing a known issue for a target
#[derive(Clone, Debug, PartialEq, serde::Serialize, serde::Deserialize)]
struct Annotation {
    text: String,
    link: Option<String>,
    // other targets that this note is relevant to
    #[serde(default)]
    also: Vec<TargetId>,
}

/// Load annotations, keyed by the target they apply to at any revision.
///
/// Keys are read as strings first: a target annotated at two revisions has two
/// keys but one id, and their notes are merged.
fn load_annotations(path: &Path) -> Result<BTreeMap<TargetId, Vec<Annotation>>, Error> {
    let raw: BTreeMap<String, Vec<Annotation>> = super::load_json_if_exists_else_default(path)?;
    let mut result = BTreeMap::<TargetId, Vec<Annotation>>::new();
    let mut add = |target, note: Annotation| {
        let notes = result.entry(target).or_default();
        if !notes.contains(&note) {
            notes.push(note);
        }
    };
    // a target's own notes come before ones it shares with other targets
    let mut shared = Vec::new();
    for (key, notes) in raw {
        let target = key.parse::<TargetId>().map_err(|e| Error::ParseJson {
            path: path.to_owned(),
            error: serde::de::Error::custom(format!("bad target '{key}': {e}")),
        })?;
        for mut note in notes {
            let also = std::mem::take(&mut note.also);
            shared.extend(also.into_iter().map(|other| (other, note.clone())));
            add(target.clone(), note);
        }
    }
    for (target, note) in shared {
        add(target, note);
    }
    Ok(result)
}

/// The previous run, compared target by target with the current one.
struct PreviousRun {
    results: DiffResults,
    /// Targets whose source repo was updated, as (previous, current)
    updated: Vec<(Target, Target)>,
}

/// Give targets in the previous run their revision in the current run.
///
/// A target whose source repo was updated keeps its id but not its sha. This
/// lets us compare it with its previous result, instead of reporting one
/// target lost and another added. An id that more than one target has in
/// either run (the same source pinned at two revisions) is left alone.
fn match_revisions(prev: DiffResults, current: &DiffResults) -> PreviousRun {
    fn unique_ids(results: &DiffResults) -> HashMap<&TargetId, Option<&Target>> {
        let mut ids = HashMap::new();
        for target in results.targets() {
            ids.entry(target.id())
                .and_modify(|seen| *seen = None)
                .or_insert(Some(target));
        }
        ids
    }
    let in_current = unique_ids(current);
    let unique_in_prev: HashSet<TargetId> = unique_ids(&prev)
        .into_iter()
        .filter_map(|(id, target)| target.map(|_| id.clone()))
        .collect();
    let mut updated = Vec::new();
    let mut match_target = |target: Target| match in_current.get(target.id()) {
        Some(Some(new)) if **new != target && unique_in_prev.contains(target.id()) => {
            updated.push((target, (*new).clone()));
            (*new).clone()
        }
        _ => target,
    };
    let success = prev
        .success
        .into_iter()
        .map(|(target, result)| (match_target(target), result))
        .collect();
    let failure = prev
        .failure
        .into_iter()
        .map(|(target, result)| (match_target(target), result))
        .collect();
    PreviousRun {
        results: crate::Results { success, failure },
        updated,
    }
}

pub(super) fn generate(target_dir: &Path, status_url: Option<&str>) -> Result<(), Error> {
    let summary_path = target_dir.join(super::SUMMARY_FILE);
    let summary: Vec<RunSummary> = crate::try_read_json(&summary_path)?;
    let sources_path = target_dir.join(super::SOURCES_FILE);
    let failures_path = target_dir.join(super::FAILED_REPOS_FILE);
    let annotations_path = target_dir.join(ANNOTATIONS_FILE);
    let sources: BTreeMap<PathBuf, String> =
        super::load_json_if_exists_else_default(&sources_path)?;
    let failures: BTreeMap<String, String> =
        super::load_json_if_exists_else_default(&failures_path)?;
    let annotations = load_annotations(&annotations_path)?;
    let accepted = AcceptedFailures::load(target_dir)?;

    let (current, prev) = match summary.as_slice() {
        [.., prev, current] => {
            let prev = match prev.try_load_results(target_dir) {
                Ok(prev) => Some(prev),
                Err(e) => {
                    log::warn!("failed to load previous run: '{e}'");
                    None
                }
            };
            (current.try_load_results(target_dir)?, prev)
        }

        [one] => (one.try_load_results(target_dir)?, None),
        [] => panic!("can't make html with no data"),
    };
    let prev = prev.map(|prev| match_revisions(prev, &current));

    let html_text = make_html(
        &summary,
        &sources,
        &current,
        prev.as_ref(),
        &failures,
        &annotations,
        &accepted,
        status_url,
    )?;
    let outpath = target_dir.join(HTML_FILE);
    crate::try_write_str(&html_text, &outpath)
}

#[allow(clippy::too_many_arguments)]
fn make_html(
    summary: &[RunSummary],
    sources: &BTreeMap<PathBuf, String>,
    current: &DiffResults,
    prev: Option<&PreviousRun>,
    repo_failures: &BTreeMap<String, String>,
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
    accepted: &AcceptedFailures,
    status_url: Option<&str>,
) -> Result<String, Error> {
    let table_body = make_table_body(summary);
    let css = include_str!("../../resources/style.css");
    let status_script = PreEscaped(include_str!("../../resources/status.js"));
    let table = html! {
        table #results {
            colgroup {
                col.date;
                col.rev;
                col.identical;
                col.total;
                col.identical_perc;
                col.fontc_err;
                col.fontmake_err;
                col.both_err;
                col.other_err;
                col.diff_perc;
                col.diff_perc_excl;
            }
            thead  {
                tr #results_head {
                    th.date scope="col" rowspan="2" { "date" }
                    th.rev scope="col" rowspan="2" { "rev" }
                    th.identical scope="col" rowspan="2" { "identical" }
                    th.total scope="col" rowspan="2" { "targets" }
                    th.identical_perc scope="col" rowspan="2"
                        title="identical targets and accepted failures, as a percentage of all targets"
                        { "matching %" }
                    th.col_group scope="colgroup" colspan="4" { "crashes" }
                    th.col_group scope="colgroup" colspan="2" { "similarity %" }
                }
                tr {
                    th.fontc_err scope="col" { "fontc" }
                    th.fontmake_err scope="col" { "fontmake" }
                    th.both_err scope="col" { "both" }
                    th.other_err scope="col" { "other" }
                    th.diff_perc scope="col" { "all" }
                    th.diff_perc_excl scope="col" { "excl crashes" }
                }
            }
            (table_body)
        }
    };
    let detailed_report = match prev {
        Some(prev) => make_detailed_report(current, &prev.results, sources, annotations, accepted),

        _ => html!(),
    };

    let weird_failures = format_repo_failures(repo_failures);
    let surprises = format_surprises(&accepted.surprises(current));
    let added_and_removed = prev
        .map(|prev| format_added_and_removed(prev, current))
        .unwrap_or_default();

    let script = PreEscaped(
        "
        <script>
        // https://developer.mozilla.org/en-US/docs/Web/API/Clipboard/writeText
        async function copyText(text) {
            try {
                await navigator.clipboard.writeText(text);
                console.log(\"Copied \" + text);
            } catch (error) {
                console.error(error.message);
            }
        }

        function showAllResults() {
            document.querySelectorAll(\".hidden_row\").forEach(e => e.classList.remove(\"hidden_row\"));
            document.querySelector(\"#show_all_row\").classList.add(\"hidden_row\");
        }

        // used to show info on adding annotations
        function openModal(target) {
            const content = createAnnotationPreBlock(target)
            document.getElementById('modalContent').innerHTML = content || 'No content provided.';
            document.getElementById('modalOverlay').classList.add('show');
        }

        function closeModal(event) {
            // Only close if clicking the overlay (not the modal content) or the close button
            if (!event || event.target.id === 'modalOverlay' || event.target.classList.contains('close-btn')) {
                document.getElementById('modalOverlay').classList.remove('show');
            }
        }

        function createAnnotationPreBlock(target) {
            const jsonObj = {
                [target]: [
                {
                    \"text\": \"your text here\",
                    \"link\": \"http://www.example.com/your_link_here\"
                }
                ]
            };

            return `<pre>${JSON.stringify(jsonObj, null, 2)}</pre>`;
        }

        // Close modal when pressing Escape key
        document.addEventListener('keydown', function(event) {
            if (event.key === 'Escape') {
                closeModal();
            }
        });
        </script>
    ",
    );

    // used to show popup for adding an annotation
    let raw_modal = PreEscaped(
        r#"
<div id="modalOverlay" class="modal-overlay" onclick="closeModal(event)">
        <div class="modal">
            To add an annotation for this target, paste the following text into
            an 'annotations.json' file in the output directory, and PR the new
                annotation to the host repository.)
            <div id="modalContent">Default content</div>
            <a href = "" onclick="event.preventDefault(); closeModal()">close [x]</a>
        </div>
    </div>
    "#,
    );

    let raw_html = html! {
        (maud::DOCTYPE)
        html {
            head {
                title { "fontc_crater results" }
                style { (css) }
                meta charset="utf-8";
                (script)
                @if status_url.is_some() {
                    script { (status_script) }
                }
            }
            body {
                h1 { "fontc_crater" }
                @if let Some(status_url) = status_url {
                    div #live_status hidden data-src=(status_url) {
                        span.text { "loading crater status" }
                        progress hidden {}
                    }
                }
                div #explain {
                    "Compiling all known Google Fonts that have sources with both "
                    {a href = "https://github.com/googlefonts/fontc" { "fontc" } }
                    " and "
                    {a href = "https://github.com/googlefonts/fontmake" { "fontmake" } }
                    ", comparing the results."
                }
                (table)
                div #explain {
                    "Jump to "
                    {a href = "#summary-report" { "summary" } }
                    ", "
                    {a href = "#diff-report" { "per-target diffs" } }
                    ", compile failures for "
                    {a href = "#fontc-failures" { "fontc only" } }
                    ", "
                    {a href = "#fontmake-failures" { "fontmake only" } }
                    ", "
                    {a href = "#both-failures" { "both compilers" } }
                    ", or "
                    {a href = "#accepted-failures" { "accepted failures" } }
                }
                (weird_failures)
                (surprises)
                (added_and_removed)
                (detailed_report)
            }
            (raw_modal)
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
    fn make_row(
        run: &RunSummary,
        prev: Option<&RunSummary>,
        is_most_recent: bool,
        default_visible: bool,
    ) -> Markup {
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
        let diff_perc_excl_diff = make_delta_decoration(
            run.stats.diff_perc_excluding_failures,
            prev.map(|p| p.stats.diff_perc_excluding_failures),
            More::IsBetter,
        );
        let diff_excl_fmt = format!("{:.3}", run.stats.diff_perc_excluding_failures);
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
        let elapsed = run
            .finished
            .signed_duration_since(run.began)
            .to_std()
            .unwrap_or_default();
        let elapsed = crate::human_readable_duration(elapsed);
        let class = (!default_visible).then_some("hidden_row");
        let matching = run.stats.identical + run.stats.accepted_failure;
        let pct = 100.0 * matching as f32 / run.stats.total_targets as f32;
        let pct = format!("{pct:.1}%");

        html! {
            tr class=[class] {
                td.date { (run.began.format("%Y-%m-%d %H%M")) span.deemphasized { " (" (elapsed) ")"} }
                td.rev { a href=(diff_url) { (short_rev) } }
                td.identical {  (run.stats.identical) " " (identical_diff)  }
                td.total {  ( run.stats.total_targets) " " (total_diff) }
                td.identical_perc {  (pct) }

                (err_cells)

                td.diff_perc {  (diff_fmt) " " (diff_perc_diff) }
                td.diff_perc_excl {  (diff_excl_fmt) " " (diff_perc_excl_diff) }
            }
        }
    }

    let iter = runs.iter().enumerate().map(|(i, run)| {
        let prev = (i > 0).then(|| &runs[i - 1]);
        let is_last = i == runs.len() - 1;
        let default_visibility = i >= runs.len().saturating_sub(16);
        make_row(run, prev, is_last, default_visibility)
    });

    html! {
        tbody {
            tr {
                td id="show_all_row" {
                    a href = "" onclick = "event.preventDefault(); showAllResults();" { "Show all " (runs.len()) " results" }
                }
            }
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
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
    accepted: &AcceptedFailures,
) -> Markup {
    let reports = [
        make_diff_report(current, prev, sources, annotations, accepted),
        make_summary_report(current),
        make_error_report(current, prev, sources, annotations, accepted),
    ];
    html! {
        @for report in reports {
            (report)
        }
    }
}

fn make_matching_families_info(
    current: &DiffResults,
    prev: &DiffResults,
    _sources: &BTreeMap<PathBuf, String>,
    accepted: &AcceptedFailures,
) -> Markup {
    let (total_families, num_perfect) = n_families_and_n_matching(current, accepted);
    let (prev_total, prev_perfect) = n_families_and_n_matching(prev, accepted);

    let total_decoration = make_delta_decoration(
        total_families as i32,
        Some(prev_total as i32),
        More::IsBetter,
    );
    let perfect_decoration = make_delta_decoration(
        num_perfect as i32,
        Some(prev_perfect as i32),
        More::IsBetter,
    );

    html! {
        (num_perfect) " " (perfect_decoration) " of " (total_families) " " (total_decoration) " total families match."

    }
}

// return the total number of families and the number that are identical or
// fail as accepted
fn n_families_and_n_matching(run: &DiffResults, accepted: &AcceptedFailures) -> (usize, usize) {
    let mut family_stats = HashMap::new();

    for (target, is_perfect) in run
        .success
        .iter()
        .map(|(targ, result)| (targ, matches!(result, DiffOutput::Identical)))
        .chain(
            run.failure
                .iter()
                .map(|(targ, err)| (targ, accepted.is_accepted(targ, err))),
        )
    {
        // all families share a config; if no config, consider all sources
        // in a given repo to be a family.
        let family = target.config_path(Path::new(""));

        let (total, num_perfect) = family_stats.entry(family).or_insert((0, 0));
        *total += 1;
        *num_perfect += is_perfect as i32;
    }

    let total_families = family_stats.len();
    let num_perfect = family_stats
        .iter()
        .filter(|(_, (total, perfect))| total == perfect)
        .count();
    (total_families, num_perfect)
}

/// Count # targets impacted by each diff type (e.g. table or table size)
fn make_summary_report(current: &DiffResults) -> Markup {
    let mut results = current
        .success
        .values()
        .filter_map(|diff| match diff {
            DiffOutput::Diffs(diffs) => Some(diffs),
            DiffOutput::Identical => None,
        })
        .flat_map(|diff| diff.iter().filter(|(k, _)| *k != "total"))
        .fold(HashMap::<&str, i32>::new(), |mut acc, e| {
            *acc.entry(e.0).or_default() += 1;
            acc
        })
        .into_iter()
        .collect::<Vec<_>>();
    results.sort_by_key(|(item, count)| (-*count, *item));

    if results.is_empty() {
        return html!();
    }

    html! {
        div.summary_report {
            h3 id="summary-report" { "Summary" }
            table {
                thead {
                    tr {
                        th { "Tag" }
                        th { "Targets with diff" }
                    }
                }
                @for (tag, count) in results {
                    tr {
                        td { (tag) }
                        td { (count) }
                    }
                }
            }
        }

    }
}

/// make the list of fonts that were both compiled successfully.
fn make_diff_report(
    current: &DiffResults,
    prev: &DiffResults,
    sources: &BTreeMap<PathBuf, String>,
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
    accepted: &AcceptedFailures,
) -> Markup {
    fn get_total_diff_ratios(results: &DiffResults) -> BTreeMap<&Target, f32> {
        results
            .success
            .iter()
            .map(|(k, v)| {
                (
                    k,
                    match v {
                        DiffOutput::Identical => 100.0,
                        DiffOutput::Diffs(d) => {
                            // sometimes the output is rounded up to 100 but is
                            // not actually identical: we want to not show that as 100!
                            let diff_perc = d.get("total").unwrap().ratio().unwrap() * 100.0;
                            diff_perc.min(99.999)
                        }
                    },
                )
            })
            .collect()
    }

    let get_repo_url = |id: &Target| {
        sources
            .get(&id.repo_dir())
            .map(String::as_str)
            .unwrap_or("#")
    };

    let current_diff = get_total_diff_ratios(current);
    let prev_diff = get_total_diff_ratios(prev);
    let mut current_diff = current_diff.into_iter().collect::<Vec<_>>();
    current_diff.sort_by_key(|(_, r)| -(r * 1e6) as i64);

    // Pre-compute short source names and find which are ambiguous
    let ambiguous_sources = {
        let mut counts = HashMap::<String, usize>::new();
        for (target, _) in &current_diff {
            let short = short_source_name(target);
            *counts.entry(short).or_default() += 1;
        }
        counts
            .into_iter()
            .filter(|(_, count)| *count > 1)
            .map(|(name, _)| name)
            .collect::<HashSet<String>>()
    };

    let mut items = Vec::new();
    for (target, ratio) in &current_diff {
        let diff_details = current.success.get(*target).unwrap();
        let prev_details = prev.success.get(*target);
        let prev_ratio = prev_diff.get(target).copied();
        // don't include 100% matches in results unless they are new
        if *ratio == 100.0 && prev_ratio == Some(100.0) {
            continue;
        }

        let repo_url = get_repo_url(target);
        let ttx_command = target.repro_command(repo_url);
        let onclick = format!("event.preventDefault(); copyText(\"{ttx_command}\");",);
        let decoration = make_delta_decoration(*ratio, prev_ratio, More::IsBetter);
        let changed_tag_list = list_different_tables(diff_details).unwrap_or_default();
        let diff_table = format_diff_report_detail_table(diff_details, prev_details);

        let annotation_list = format_annotations(target, annotations);

        let details = html! {
            div.diff_info {
                (diff_table)
                a href = (repo_url) { "view source repository" }
                " "
                a href = "" onclick = (onclick) { "copy reproduction command" }
                " "
                (annotation_list)
            }
        };

        // avoid .9995 printing as 100%
        let ratio_fmt = format!("{:.3}%", (ratio * 1000.0).floor() / 1000.0);
        let maybe_annotation_icon = if annotations.contains_key(target.id()) {
            " ✎"
        } else {
            ""
        };
        let target_description = make_target_description(target, &ambiguous_sources);

        items.push(html! {
            details {
                summary {
                    span.font_path {
                        (target_description) (maybe_annotation_icon)
                    }
                    span.diff_result { (ratio_fmt) " " (decoration) }
                    span.changed_tag_list { (changed_tag_list) }
                }
                (details)
            }
        });
    }

    let matching_familes = make_matching_families_info(current, prev, sources, accepted);
    html! {
        div.diff_report {
            h3 id="diff-report" { "Diffs" }
            div.matching_families { (matching_familes) }
            @for item in items {
                (item)
            }
        }
    }
}

fn format_annotations(
    target: &Target,
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
) -> Markup {
    let Some(annotations) = annotations.get(target.id()) else {
        let target = target.id().to_string();
        let onclick = format!("event.preventDefault(); openModal(\"{target}\");",);
        return html! {
            a href = ("#") onclick = (onclick) { "new annotation" }

        };
    };
    let make_link_part = |annotation: &Annotation| {
        if let Some(link) = annotation.link.as_ref() {
            html! {
               ": " a href = (link) { (link) }
            }
        } else {
            Default::default()
        }
    };
    html! {
        div.annotations {
            h5 { "annotations" }
            ul {
                @for annotation in annotations {
                    li {
                        (annotation.text) (make_link_part(annotation))
                    }
                }
                li { em {"(to add or change, manually edit annotations.json)"  } }
            }
        }
    }
}

/// Compute the short source name for a target (just the filename).
fn short_source_name(target: &Target) -> String {
    let bare_path = target.source_path(Path::new(""));
    bare_path.file_name().unwrap().to_string_lossy().to_string()
}

fn make_target_description(target: &Target, ambiguous_sources: &HashSet<String>) -> Markup {
    let short = short_source_name(target);
    // If the short name is ambiguous (used by multiple targets), prepend repo
    // name to disambiguate (e.g. "myrepo/font.glyphs" instead of "font.glyphs").
    let source = if ambiguous_sources.contains(&short) {
        format!("{}/{short}", target.repo_name())
    } else {
        short
    };
    let annotation = match target.build() {
        BuildType::Default => "default".to_string(),
        BuildType::GfTools if target.config().file_stem() != Some(OsStr::new("config")) => {
            format!("gftools, {}", target.config().display())
        }
        _ => "gftools".to_string(),
    };
    html! {
        (source)
            " "
            em { "(" (annotation) ")" }
    }
}

fn make_error_report(
    current: &DiffResults,
    prev: &DiffResults,
    sources: &BTreeMap<PathBuf, String>,
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
    accepted: &AcceptedFailures,
) -> Markup {
    let current_fontc = get_compiler_failures(current, accepted, "fontc");
    let prev_fontc = get_compiler_failures(prev, accepted, "fontc");
    let current_fontmake = get_compiler_failures(current, accepted, "fontmake");
    let prev_fontmake = get_compiler_failures(prev, accepted, "fontmake");
    let current_accepted = get_accepted_failures(current, accepted);
    let prev_accepted = get_accepted_failures(prev, accepted);

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

    let fontc = if current_fontc.len() - current_both.len() > 0 {
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
            annotations,
        )
    } else {
        Default::default()
    };

    let fontmake = if current_fontmake.len() - current_both.len() > 0 {
        make_error_report_group(
            "fontmake",
            current_fontmake
                .keys()
                .copied()
                .filter(|k| !current_both.contains(k))
                .map(|k| (k, !prev_fontmake.contains_key(k))),
            |path| {
                current_fontmake
                    .get(path)
                    .copied()
                    .map(format_compiler_error)
                    .unwrap_or_default()
            },
            sources,
            annotations,
        )
    } else {
        Default::default()
    };

    let both = if !current_both.is_empty() {
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
            annotations,
        )
    } else {
        Default::default()
    };

    let accepted_report = if !current_accepted.is_empty() {
        make_error_report_group(
            "accepted",
            current_accepted
                .keys()
                .copied()
                .map(|k| (k, !prev_accepted.contains_key(k))),
            |path| {
                let Some((failed, accepted)) = current_accepted.get(path) else {
                    return Default::default();
                };
                html! {
                    p { (format_accepted_reason(accepted)) }
                    @if let Some(err) = &failed.fontc {
                        .h5 { "fontc" }
                        (format_compiler_error(err))
                    }
                    @if let Some(err) = &failed.fontmake {
                        .h5 { "fontmake" }
                        (format_compiler_error(err))
                    }
                }
            },
            sources,
            annotations,
        )
    } else {
        Default::default()
    };

    let other = if !current_other.is_empty() {
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
            annotations,
        )
    } else {
        Default::default()
    };

    html! {
        (fontc)
        (fontmake)
        (both)
        (other)
        (accepted_report)
    }
}

fn format_compiler_error(err: &CompilerFailure) -> Markup {
    html! {
            div.backtrace {
            div.stderr { (err.stderr) }

        }
    }
}

fn list_different_tables(current: &DiffOutput) -> Option<String> {
    let changed = current
        .iter_tables()
        .filter(|x| *x != "total")
        .collect::<Vec<_>>()
        .join(", ");
    if changed.is_empty() {
        return None;
    }
    Some(format!("({changed})"))
}

/// for a given diff, the detailed information on per-table changes
fn format_diff_report_detail_table(current: &DiffOutput, prev: Option<&DiffOutput>) -> Markup {
    let value_decoration = |table: &str, value: DiffValue| -> Markup {
        let prev_value = match prev {
            None => None,
            Some(DiffOutput::Identical) => Some(DiffValue::Ratio(1.0)),
            Some(DiffOutput::Diffs(tables)) => tables.get(table).cloned(),
        };

        // size reporting is sort of hacked on, and doesn't really follow the same pattern
        // as table diffs; the diff % we show in this case is the ratio between the two
        // sizes. For instance a value of +60% means fontmake is 40% the size of fontc.
        // negative numbers mean fontc is smaller than fontmake.
        if table.starts_with("sizeof") {
            if let (DiffValue::Ratio(r), Some(DiffValue::Ratio(prev))) =
                (&value, prev_value.as_ref())
            {
                return make_delta_decoration(
                    r.abs() as i32,
                    Some(prev.abs() as i32),
                    More::IsWorse,
                );
            }
            return Default::default();
        }

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

    if all_items.is_empty() {
        html!()
    } else {
        html! {
            table {
                thead {
                    tr {
                        th.diff_table scope = "col" { "table" }
                        th.diff_value scope = "col" { "value" }
                    }
                }
                @for (table, value) in all_items {
                    tr.table_diff_row {
                        td.table_diff_name { (table) }
                        @if table.starts_with("sizeof") {

                            td.table_diff_value {
                                (value.as_n_of_bytes()) "B " ( {value_decoration(table, value) })
                            }
                        } @else {
                            td.table_diff_value { (value) " " ( {value_decoration(table, value) }) }
                        }
                    }
                }
            }
        }
    }
}

fn make_error_report_group<'a>(
    group_name: &str,
    paths_and_if_is_new_error: impl Iterator<Item = (&'a Target, bool)>,
    details: impl Fn(&Target) -> Markup,
    sources: &BTreeMap<PathBuf, String>,
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
) -> Markup {
    let items =
        make_error_report_group_items(paths_and_if_is_new_error, details, sources, annotations);

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
    paths_and_if_is_new_error: impl Iterator<Item = (&'a Target, bool)>,
    details: impl Fn(&Target) -> Markup,
    sources: &BTreeMap<PathBuf, String>,
    annotations: &BTreeMap<TargetId, Vec<Annotation>>,
) -> Markup {
    let get_repo_url = |id: &Target| {
        sources
            .get(&id.repo_dir())
            .map(String::as_str)
            .unwrap_or("#")
    };
    let make_repro_command = |target: &Target| {
        let url = get_repo_url(target);
        let ttx_command = target.repro_command(url);
        format!("event.preventDefault(); copyText(\"{ttx_command}\");",)
    };
    html! {
        @for (path, is_new) in paths_and_if_is_new_error {
            @let maybe_annotation_icon = if annotations.contains_key(path.id()) { " ✎" } else { "" };
            @let annotation_list = format_annotations(path, annotations);
            details.report_group_item {
                summary {
                    (path)
                     @if is_new { " 🆕" }
                     (maybe_annotation_icon)
            }
                    div.diff_info {
                        a href = (get_repo_url(path)) { "view source repository" }
                        " "
                        a href = "" onclick = (make_repro_command(path)) { "copy reproduction command" }
                        " "
                        (annotation_list)
                    }

                (details(path))
            }
        }
    }
}
fn get_other_failures(results: &DiffResults) -> BTreeMap<&Target, &str> {
    results
        .failure
        .iter()
        .filter_map(|(id, r)| match r {
            DiffError::CompileFailed(_) => None,
            DiffError::Other(err) => Some((id, err.as_str())),
        })
        .collect()
}

fn format_accepted_reason(accepted: &AcceptedFailure) -> Markup {
    html! {
        (accepted.reason)
        @if let Some(link) = &accepted.link {
            " (" a href = (link) { (link) } ")"
        }
    }
}

// accepted failures that passed, failed differently, or weren't run
fn format_surprises(surprises: &[(&TargetId, &AcceptedFailure, Surprise)]) -> Markup {
    if surprises.is_empty() {
        return Default::default();
    }
    let describe = |surprise: &Surprise| match surprise {
        Surprise::Passed => "it compiled".to_string(),
        Surprise::FailedDifferently(DiffError::CompileFailed(failed)) => {
            match FailingCompiler::of(failed) {
                Some(FailingCompiler::Both) => "both failed".to_string(),
                Some(one) => format!("only {} failed", one.name()),
                None => "it hit some other error".to_string(),
            }
        }
        Surprise::FailedDifferently(DiffError::Other(_)) => "it hit some other error".to_string(),
        Surprise::NotRun => "it was not in this run".to_string(),
    };
    html! {
        div.surprises {
            h4 { (surprises.len()) " accepted failures have changed" }
            ul {
                @for (target, accepted, surprise) in surprises {
                    li {
                        (target) ": accepted as failing in " (accepted.fails.name()) ", but "
                        (describe(surprise)) ". " (format_accepted_reason(accepted))
                    }
                }
            }
        }
    }
}

// failures that result from us not being able to access a repo in the target list
fn format_repo_failures(failures: &BTreeMap<String, String>) -> Markup {
    if failures.is_empty() {
        return Default::default();
    }

    html! {
        div.repo_failures {
            h4 { "failed to find targets for " (failures.len()) " repos" }
            ul {
                @for (repo, reason) in failures {
                    li {
                        (repo) ": '" (reason) "'"
                    }
                }
            }
        }
    }
}

fn format_added_and_removed(prev: &PreviousRun, current: &DiffResults) -> Markup {
    let all_prev = prev.results.targets().collect::<BTreeSet<_>>();
    let all_current = current.targets().collect::<BTreeSet<_>>();

    let added = all_current.difference(&all_prev).collect::<BTreeSet<_>>();
    let removed = all_prev.difference(&all_current).collect::<BTreeSet<_>>();

    let added_list = if !added.is_empty() {
        html! {
            h4 {"Added "(added.len()) " new targets"}
            ul {
                @for target in added {
                    li {
                        (target)

                    }

                }
            }
        }
    } else {
        Default::default()
    };

    let removed_list = if !removed.is_empty() {
        html! {
            h4 {"Lost "(removed.len()) " old targets"}
            ul {
                @for target in removed {
                    li {
                        (target)

                    }

                }
            }
        }
    } else {
        Default::default()
    };

    let mut updated_repos = BTreeMap::<_, usize>::new();
    for (old, new) in &prev.updated {
        *updated_repos
            .entry((new.id().repo(), old.sha(), new.sha()))
            .or_default() += 1;
    }
    let updated_list = if !prev.updated.is_empty() {
        html! {
            h4 {"Updated sources for "(prev.updated.len()) " targets"}
            ul {
                @for ((repo, old, new), n) in updated_repos {
                    li {
                        (repo.display()) ": " (old) " → " (new) " (" (n) " targets)"
                    }
                }
            }
        }
    } else {
        Default::default()
    };

    html! {
        (added_list)
        (removed_list)
        (updated_list)
    }
}

fn get_compiler_failures<'a>(
    results: &'a DiffResults,
    accepted: &AcceptedFailures,
    compiler: &str,
) -> BTreeMap<&'a Target, &'a CompilerFailure> {
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
        .filter(|(id, r)| !accepted.is_accepted(id, r))
        .flat_map(|(id, r)| get_err(r).map(|e| (id, e)))
        .collect()
}

fn get_accepted_failures<'a>(
    results: &'a DiffResults,
    accepted: &'a AcceptedFailures,
) -> BTreeMap<&'a Target, (&'a CompileFailed, &'a AcceptedFailure)> {
    results
        .failure
        .iter()
        .filter_map(|(id, r)| match r {
            DiffError::CompileFailed(failed) if accepted.is_accepted(id, r) => {
                Some((id, (failed, accepted.get(id)?)))
            }
            _ => None,
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    const ASAP_OLD: &str = "Omnibus-Type/Asap/sources/config.yaml Asap.glyphs?2de32f20d7 (default)";
    const ASAP_NEW: &str = "Omnibus-Type/Asap/sources/config.yaml Asap.glyphs?927ab390d4 (default)";
    const DERP_OLD: &str = "org/derp/sources/config.yaml Derp.glyphs?0123456789 (default)";
    const DERP_NEW: &str = "org/derp/sources/config.yaml Derp.glyphs?abcdef0123 (default)";

    // Target only deserializes from borrowed strings, so go through text
    fn identical(targets: &[&str]) -> DiffResults {
        let success: serde_json::Map<_, _> = targets
            .iter()
            .map(|t| (t.to_string(), json!("identical")))
            .collect();
        serde_json::from_str(&json!({ "success": success, "failure": {} }).to_string()).unwrap()
    }

    #[test]
    fn updated_source_is_matched_with_previous_result() {
        let prev = identical(&[DERP_OLD]);
        let current = identical(&[DERP_NEW]);
        let prev = match_revisions(prev, &current);

        let new: Target = DERP_NEW.parse().unwrap();
        assert!(prev.results.success.contains_key(&new));
        assert_eq!(prev.updated.len(), 1);
        assert_eq!(prev.updated[0].0.sha(), "0123456789");
    }

    #[test]
    fn source_pinned_at_two_revisions_is_left_alone() {
        let prev = identical(&[ASAP_OLD, DERP_OLD]);
        let current = identical(&[ASAP_OLD, ASAP_NEW, DERP_OLD]);
        let prev = match_revisions(prev, &current);

        let old: Target = ASAP_OLD.parse().unwrap();
        assert!(prev.results.success.contains_key(&old));
        assert!(prev.updated.is_empty());
    }

    #[test]
    fn annotations_merge_across_revisions() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join(ANNOTATIONS_FILE);
        let annotations = json!({
            DERP_OLD: [{ "text": "same", "link": null }, { "text": "old only", "link": null }],
            DERP_NEW: [{ "text": "same", "link": null }],
            "org/other/sources/config.yaml Other.glyphs (default)": [
                { "text": "shared", "link": null, "also": [DERP_NEW] }
            ],
        });
        std::fs::write(&path, annotations.to_string()).unwrap();

        let loaded = load_annotations(&path).unwrap();
        let derp: Target = DERP_NEW.parse().unwrap();
        let texts = loaded[derp.id()]
            .iter()
            .map(|note| note.text.as_str())
            .collect::<Vec<_>>();
        assert_eq!(texts, ["same", "old only", "shared"]);
    }
}
