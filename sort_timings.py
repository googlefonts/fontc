#!/usr/bin/env python3
"""Parse and sort timing data from fontc_crater log files."""

import re
import sys
from pathlib import Path

def parse_timings(log_file):
    """Extract timing entries from log file."""
    timings = []
    # Pattern: [TIMING] step_name: X.XXXs
    pattern = r'\[TIMING\]\s+(\w+):\s+([\d.]+)s'

    with open(log_file) as f:
        for line in f:
            match = re.search(pattern, line)
            if match:
                step_name = match.group(1)
                duration = float(match.group(2))
                # Extract target name from the log line
                # Format: ... target/path: [TIMING] ...
                target_match = re.search(r']\s+([^:]+):\s+\[TIMING\]', line)
                target = target_match.group(1) if target_match else "unknown"
                timings.append((duration, step_name, target))

    return timings

def main():
    log_file = sys.argv[1] if len(sys.argv) > 1 else "target/crater/crater_20260321_165152.log"
    log_path = Path(log_file)

    if not log_path.exists():
        print(f"Error: Log file not found: {log_file}")
        print("Usage: python sort_timings.py <log_file>")
        sys.exit(1)

    timings = parse_timings(log_path)

    # Sort by duration (descending)
    timings.sort(reverse=True)

    print(f"\nTiming results from {log_file}:")
    print(f"{'Duration':>10}  {'Step':<20}  {'Target'}")
    print("-" * 100)

    for duration, step, target in timings:
        print(f"{duration:>10.3f}s  {step:<20}  {target}")

    # Summary by step type
    print("\n" + "=" * 100)
    print("Summary by step type:")
    step_totals = {}
    for duration, step, target in timings:
        step_totals[step] = step_totals.get(step, 0) + duration

    sorted_steps = sorted(step_totals.items(), key=lambda x: x[1], reverse=True)
    for step, total in sorted_steps:
        count = sum(1 for d, s, t in timings if s == step)
        avg = total / count if count > 0 else 0
        print(f"  {step:<20}  total: {total:>10.3f}s  count: {count:>3}  avg: {avg:>10.3f}s")

    print(f"\nTotal time across all steps: {sum(t[0] for t in timings):.3f}s")

if __name__ == "__main__":
    main()
