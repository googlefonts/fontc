// Live status of the current or most recent crater run.
//
// run.sh (in the fontc_crater repo) publishes status.json to the 'status'
// branch while it runs; #live_status's data-src is its url:
//
//   {"state": "running"|"finished", "started": <iso8601>, "updated": <iso8601>,
//    "finished": <iso8601>|null, "exit_code": <int>|null, "progress": <...>|null}
//
// where progress is the contents of crater's --progress-file:
//
//   {"phase": "preparing"|"skipped", "fontc_rev": <rev>, "compare_url": <url>}
//   {"phase": "running", "done": n, "total": n, "fontc_rev": <rev>, "compare_url": <url>}
//
// compare_url is the link the run gets in the report's list of runs.

const STATUS_POLL_MS = 60 * 1000;
// run.sh republishes every minute and the CDN caches for five
const STATUS_STALE_MS = 15 * 60 * 1000;

// returns { status } or, if there's no status to show, { error }
async function fetchStatus(url) {
    let response;
    try {
        response = await fetch(url, { cache: "no-store" });
    } catch {
        return { error: "couldn't reach GitHub to load crater's status" };
    }
    if (response.status === 404) {
        return { error: "crater hasn't published a status yet" };
    }
    if (!response.ok) {
        return { error: `couldn't load crater's status (HTTP ${response.status})` };
    }
    try {
        return { status: await response.json() };
    } catch {
        return { error: "crater's status couldn't be read" };
    }
}

function formatDuration(ms) {
    const minutes = Math.max(0, Math.round(ms / 60000));
    if (minutes < 60) return `${minutes} min`;
    const hours = Math.floor(minutes / 60);
    if (hours < 48) return `${hours}h ${minutes % 60}m`;
    return `${Math.floor(hours / 24)} days`;
}

function since(isoDate) {
    return `${formatDuration(Date.now() - Date.parse(isoDate))} ago`;
}

// the returned text is a list of strings and links ({ text, href })
function describeStatus(status) {
    const progress = status.progress;
    // the fontc rev being tested, once crater has said
    const rev = progress?.fontc_rev ? { text: progress.fontc_rev, href: progress.compare_url } : null;
    if (status.state === "running") {
        const quietFor = Date.now() - Date.parse(status.updated);
        if (quietFor > STATUS_STALE_MS) {
            return {
                state: "failed",
                text: [
                    ...(rev ? ["crater looks stuck testing fontc ", rev] : ["crater looks stuck"]),
                    `: started ${since(status.started)}, no update for ${formatDuration(quietFor)}`,
                ],
            };
        }
        const testing = rev ? ["crater is testing fontc ", rev] : ["crater is running"];
        // as of this snapshot, not now, so that it agrees with the count
        const elapsed = `${formatDuration(Date.parse(status.updated) - Date.parse(status.started))} in`;
        if (progress?.phase === "running") {
            return {
                state: "running",
                text: [...testing, `: ${elapsed}, ${progress.done} of ${progress.total} targets`],
                progress,
            };
        }
        return { state: "running", text: [...testing, `: ${elapsed}, preparing`] };
    }
    const lastRun = rev ? ["last crater run of fontc ", rev] : ["last crater run"];
    const took = formatDuration(Date.parse(status.finished) - Date.parse(status.started));
    if (status.exit_code !== 0) {
        return {
            state: "failed",
            text: [...lastRun, ` failed ${since(status.finished)} (exit code ${status.exit_code}, after ${took})`],
        };
    }
    if (progress?.phase === "skipped") {
        // the rev is the same as the last run's, already in the report
        return { state: "ok", text: [`last crater run ${since(status.finished)} found nothing new to test`] };
    }
    return { state: "ok", text: [...lastRun, ` finished ${since(status.finished)}, after ${took}`] };
}

function toNode(part) {
    if (typeof part === "string") return part;
    const link = document.createElement("a");
    link.href = part.href;
    link.textContent = part.text;
    return link;
}

async function updateStatus() {
    const container = document.getElementById("live_status");
    const { status, error } = await fetchStatus(container.dataset.src);
    const described = error ? { state: "unknown", text: [error] } : describeStatus(status);
    container.hidden = false;
    container.className = described.state;
    container.title = error
        ? container.dataset.src
        : `started ${status.started}, last update ${status.updated}`;
    container.querySelector(".text").replaceChildren(...described.text.map(toNode));
    const bar = container.querySelector("progress");
    bar.hidden = !described.progress;
    if (described.progress) {
        bar.max = described.progress.total;
        bar.value = described.progress.done;
    }
}

document.addEventListener("DOMContentLoaded", function () {
    if (document.getElementById("live_status")) {
        updateStatus();
        setInterval(updateStatus, STATUS_POLL_MS);
    }
});
