#!/usr/bin/env python3
"""Parse fontc_crater log file and print slowest 10 items."""

import re
from pathlib import Path


def parse_time(time_str: str) -> float:
    """Convert time string like '0m0.32s' to seconds."""
    match = re.match(r"(\d+)m([\d.]+)s", time_str)
    if match:
        minutes = int(match.group(1))
        seconds = float(match.group(2))
        return minutes * 60 + seconds
    return 0.0


def parse_log_line(line: str) -> dict | None:
    """Parse a log line with timing info.
    
    Format: [timestamp] DEBUG fontc_crater] finished <config> <source>?<hash> (<mode>) in <time> (<active>)
    """
    # Match lines with "finished" and timing
    pattern = r"finished\s+(.+?)\s+(.+?)\?[a-f0-9]+\s+\([^)]+\)\s+in\s+(\d+m[\d.]+s)"
    match = re.search(pattern, line)
    
    if match:
        config = match.group(1)
        source = match.group(2)
        time_str = match.group(3)
        time_seconds = parse_time(time_str)
        
        return {
            "config": config,
            "source": source,
            "time_str": time_str,
            "time_seconds": time_seconds,
        }
    
    return None


def main():
    log_file = Path("crater_20260321_154613.log")
    
    if not log_file.exists():
        print(f"Error: {log_file} not found")
        return
    
    items = []
    
    with open(log_file, "r") as f:
        for line in f:
            item = parse_log_line(line)
            if item:
                items.append(item)
    
    # Sort by time (descending) and get top 10
    items.sort(key=lambda x: x["time_seconds"], reverse=True)
    top_10 = items[:10]
    
    print("Slowest 10 items:")
    print("-" * 80)
    for i, item in enumerate(top_10, 1):
        print(f"{i}. Time: {item['time_str']:>8} | Config: {item['config']}")
        print(f"   Source: {item['source']}")
        print()


if __name__ == "__main__":
    main()
