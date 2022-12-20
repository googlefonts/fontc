"""Fetch and compile test sources.
"""
from absl import app
from pathlib import Path
import subprocess
import sys
from typing import Tuple

_TRY_ME = (
    # Glyphs 2
    ("https://github.com/Omnibus-Type/Texturina", ("sources/Texturina.glyphs", "sources/Texturina-Italic.glyphs")),
    # Glyphs 3
    ("https://github.com/googlefonts/lexend", ("sources/Lexend.glyphs",)),
    # .designspace + UFO
    ("https://github.com/xconsau/KumbhSans", ("sources/KumbhSans.designspace",)),
)

def curr_dir() -> Path:
    return Path(".").resolve()


def repo_dir(repo_url) -> Path:
    return curr_dir() / "build" / repo_url.split("/")[-1].lower()


def fontc_command(source) -> Tuple[str, ...]:
    return (
        "cargo",
        "run",
        "--package",
        "fontc",
        "--",
        "--source",
        str(source)
    )


def run(cmd):
    print("  " + " ".join(str(c) for c in cmd))
    result = subprocess.run(cmd, text=True, capture_output=True)
    if result.returncode != 0:
        print("  failed with code", result.returncode)
        print(result.stderr)
        print(result.stdout)
        
    return result.returncode


def main(argv):
    for (repo_url, source_files) in _TRY_ME:
        clone_dir = repo_dir(repo_url).relative_to(curr_dir())
        print(f"{repo_url} in {clone_dir}")
        
        if (clone_dir / ".git").is_dir():
            git_cmd = ("git", "-C", clone_dir, "pull")
        else:
            git_cmd = ("git", "clone", repo_url, clone_dir)
            clone_dir.mkdir(parents=True, exist_ok=True)    
        if run(git_cmd) != 0:
            continue

        for source_file in source_files:            
            source_file = clone_dir / source_file
            if not source_file.is_file():
                print(f"  {source_file} FAIL, no such file")
                continue

            if run(fontc_command(source_file)) != 0:
                continue

            print("  SUCCESS")

if __name__ == "__main__":
    app.run(main)