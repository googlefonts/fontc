// Live status of the current or most recent crater run.
//
// run.sh (in the fontc_crater repo) publishes status.json to the 'status'
// branch while it runs; #live_status's data-src is its url:
//
//   {"state": "running"|"finished", "started": <iso8601>, "updated": <iso8601>,
//    "finished": <iso8601>|null, "exit_code": <int>|null, "progress": <...>|null}
//
// where progress is the contents of crater's --progress-file: {"phase":
// "running", "done": n, "total": n} or {"phase": "skipped"}.

const STATUS_POLL_MS = 60 * 1000;
// run.sh republishes every minute and the CDN caches for five
const STATUS_STALE_MS = 15 * 60 * 1000;

async function fetchJson(url) {
    try {
        const response = await fetch(url, { cache: "no-store" });
        return response.ok ? await response.json() : null;
    } catch {
        return null;
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

function describeStatus(status) {
    const progress = status.progress;
    if (status.state === "running") {
        const quietFor = Date.now() - Date.parse(status.updated);
        if (quietFor > STATUS_STALE_MS) {
            return {
                state: "failed",
                text: `crater looks stuck: started ${since(status.started)}, no update for ${formatDuration(quietFor)}`,
            };
        }
        const started = `crater is running, started ${since(status.started)}`;
        if (progress?.phase === "running") {
            return { state: "running", text: `${started}: ${progress.done} of ${progress.total} targets`, progress };
        }
        return { state: "running", text: `${started}: preparing` };
    }
    const took = formatDuration(Date.parse(status.finished) - Date.parse(status.started));
    if (status.exit_code !== 0) {
        return {
            state: "failed",
            text: `last crater run failed ${since(status.finished)} (exit code ${status.exit_code}, after ${took})`,
        };
    }
    if (progress?.phase === "skipped") {
        return { state: "ok", text: `last crater run ${since(status.finished)} found nothing new to test` };
    }
    return { state: "ok", text: `last crater run finished ${since(status.finished)}, after ${took}` };
}

async function updateStatus() {
    const container = document.getElementById("live_status");
    const status = await fetchJson(container.dataset.src);
    if (!status) {
        container.hidden = true;
        return;
    }
    const described = describeStatus(status);
    container.hidden = false;
    container.className = described.state;
    container.title = `started ${status.started}, last update ${status.updated}`;
    container.querySelector(".text").textContent = described.text;
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
