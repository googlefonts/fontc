#!/usr/bin/env python3
"""Helper for comparing the build time of two branches of fontc

Turns each into ttx, eliminates expected sources of difference and prints
a brief summary of the result.

fontmake should be installed in an active virtual environment.

Usage:
    # On a branch with changes we think are a speedup
    python resources/scripts/time_build.py ../OswaldFont/sources/Oswald.glyphs
"""

from absl import app
from absl import flags
from pathlib import Path
import time
from typing import MutableSequence
import statistics
import subprocess
import sys


FLAGS = flags.FLAGS


flags.DEFINE_string("baseline", "main", "The branch to use as baseline.")
flags.DEFINE_string(
    "branch", None, "The branch with changes. If unset, the current branch."
)
flags.DEFINE_integer(
    "warmup", 2, "How many times to build and ignore results, per branch."
)
flags.DEFINE_integer(
    "reps", 10, "How many times to build and track results, per branch."
)
flags.DEFINE_bool(
    "print_cmds", False, "Whether to print the subprocess spawn commands."
)


def run(cmd: MutableSequence, **kwargs) -> str:
    cmd = tuple(str(s) for s in cmd)
    if FLAGS.print_cmds:
        cmd_string = " ".join(cmd)
        print(cmd_string)
    result = subprocess.run(
        cmd,
        text=True,
        check=True,
        capture_output=True,
        **kwargs,
    )
    return result


def current_branch() -> str:
    return run(("git", "rev-parse", "--abbrev-ref", "HEAD")).stdout.strip()


def compile_time(source):
    # crude but we're looking for large deltas here
    start = time.perf_counter()
    run(("target/release/fontc", source))
    end = time.perf_counter()
    return end - start


def run_branch(branch, warmup, reps, source):
    print(f"Switching to {branch}...")
    run(("git", "checkout", branch))
    print(f"Compiling the compiler...")
    run(("cargo", "build", "--release"))
    print(f"Clearing build/")
    run(("rm", "-rf", "build/"))

    print(f"Warming up...")
    [compile_time(source) for _ in range(warmup)]
    print(f"Capturing...")
    times = [compile_time(source) for _ in range(reps)]
    return (statistics.mean(times), statistics.stdev(times))


def main(argv):
    initial_branch = current_branch()
    baseline_branch = FLAGS.baseline
    changed_branch = FLAGS.branch
    if changed_branch is None:
        changed_branch = current_branch()

    if baseline_branch == changed_branch:
        sys.exit(
            f"Comparing {baseline_branch} to itself is meainingless, perhaps checkout the branch with changes?"
        )

    if len(argv) != 2:
        sys.exit("Pass one argument, the path to a source to build")

    source = Path(argv[1])
    if not source.is_file():
        sys.exit(f"{source} is not a file")

    print(f"Compare {changed_branch} to {baseline_branch} running ${argv}")
    print(f"Warmup {FLAGS.warmup}, reps {FLAGS.reps}")

    (base_mean, base_stdev) = run_branch(
        baseline_branch, FLAGS.warmup, FLAGS.reps, source
    )
    (changed_mean, changed_stdev) = run_branch(
        changed_branch, FLAGS.warmup, FLAGS.reps, source
    )

    print("branch mean stdev")
    for (b, m, s) in [
        (baseline_branch, base_mean, base_stdev),
        (changed_branch, changed_mean, changed_stdev),
    ]:
        print(f"{b} {m:.03} {s:.03}")

    run(("git", "checkout", initial_branch))


if __name__ == "__main__":
    app.run(main)
