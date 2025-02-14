"""
Removes elements from .glyphs files that we don't care about.
"""

from pathlib import Path
import re
import sys


_EXEMPTIONS = {
    ("Dated.glyphs", "date"),
}

_DELETE_ME = {
    ".appVersion",
    "DisplayStrings"
    "date"
    "lastChange"
}


def delete_key(plist_file: Path, plist: str, key: str) -> str:
    if (plist_file.name, key) in _EXEMPTIONS:
        print("skip", plist_file, key)
        return plist
    return re.sub(f"(?sm)^{key} = .*?;\n", "", plist)


def main(argv):
    exit_code = 0
    just_check = any(v == "just_check" for v in argv)
    testdata_dir = Path(__file__).parent.parent / "testdata"
    assert testdata_dir.is_dir(), testdata_dir

    plist_files = set()
    for subdir in ("glyphs2", "glyphs3"):
        for pat in ("*.glyphs", "*.plist"):
            plist_files.update((testdata_dir / subdir).rglob(pat))

    assert len(plist_files) > 0
    
    for plist_file in plist_files:
        original_plist = plist_file.read_text()
        plist = original_plist

        for key in _DELETE_ME:
            plist = delete_key(plist_file, plist, key)
        
        if plist != original_plist:
            if not just_check:
                plist_file.write_text(plist)
            else:
                print(plist_file, "is not tidy")
            exit_code = 1

    sys.exit(exit_code)
    

if __name__ == "__main__":
    main(sys.argv)