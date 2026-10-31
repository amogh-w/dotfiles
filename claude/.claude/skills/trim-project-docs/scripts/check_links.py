#!/usr/bin/env python3
"""Resolve every markdown link and heading anchor in a repo's tracked docs.

    check_links.py [ROOT]          # default: current directory
    check_links.py --quiet         # only print findings

Exit non-zero on any real break, so it can gate a commit.

WHY THIS EXISTS
---------------
A deletion pass is what breaks links, and it is the pass least likely to look.
Trimming a doc removes headings other files anchor to, and deleting a file
orphans every link pointing at it -- neither shows up in the diff of the file
being edited. Run this after any trim, before reporting the trim complete.

WHAT IS NOT A BREAK
-------------------
Template skeletons legitimately link to files they instruct you to create
(`samples.md`, `finetuning.md` in a `_template/` folder). Those are counted and
reported separately rather than failing the run -- an audit that cries wolf on
scaffolding trains its reader to ignore it.
"""
import argparse
import re
import subprocess
import sys
from pathlib import Path

LINK = re.compile(r"\[([^\]]*)\]\(\s*([^)\s]+?)\s*(?:\"[^\"]*\")?\s*\)")
HEADING = re.compile(r"^#+\s+(.+?)\s*$", re.M)
# SKIP_DIRS is duplicated in find_steering_docs.sh (SKIP_NAMES), trimstate.py
# and find_self_referential.py -- keep all four in sync by hand.
SKIP_DIRS = {".git", "node_modules", ".venv", "venv", "vendor", "target",
             "dist", "build", "__pycache__", ".next", ".tox", "site-packages"}


def slug(text: str) -> str:
    """GitHub-style heading anchor: lowercase, punctuation dropped, spaces to -."""
    t = re.sub(r"`|\*|_", "", text.lower())
    t = re.sub(r"[^a-z0-9 \-]", "", t)
    return t.strip().replace(" ", "-")


def docs(root: Path) -> list[Path]:
    try:
        out = subprocess.run(["git", "ls-files", "--cached", "--others",
                              "--exclude-standard"],
                             cwd=root, capture_output=True, text=True, timeout=30)
        if out.returncode == 0:
            names = [n for n in out.stdout.splitlines() if n.endswith(".md")]
            return [root / n for n in names]
    except (OSError, subprocess.SubprocessError):
        pass
    return [p for p in root.rglob("*.md")
            if not any(x in SKIP_DIRS for x in p.relative_to(root).parts)]


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("root", nargs="?", default=".")
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args()
    root = Path(args.root).resolve()

    files = [f for f in docs(root) if f.is_file()]
    anchors: dict[Path, set[str]] = {}

    def heads(p: Path) -> set[str]:
        if p not in anchors:
            try:
                anchors[p] = {slug(h) for h in HEADING.findall(
                    p.read_text(errors="replace"))}
            except OSError:
                anchors[p] = set()
        return anchors[p]

    broken, bad_anchor, scaffold = [], [], 0
    for f in files:
        text = f.read_text(errors="replace")
        rel = f.relative_to(root)
        for _, target in LINK.findall(text):
            if target.startswith(("http://", "https://", "mailto:", "#!")):
                continue
            path, _, frag = target.partition("#")
            # A template's skeleton links to files it tells you to create.
            if path and "_template" in str(rel) and not path.startswith("."):
                scaffold += 1
                continue
            tgt = f if not path else (f.parent / path)
            try:
                tgt = tgt.resolve()
            except OSError:
                broken.append((rel, target)); continue
            if not tgt.exists():
                broken.append((rel, target))
            elif frag and tgt.is_file() and slug(frag) not in heads(tgt):
                bad_anchor.append((rel, target))

    if not args.quiet:
        print(f"checked {len(files)} markdown file(s) under {root}")
        if scaffold:
            print(f"  ({scaffold} template-scaffold link(s) skipped by design)")

    if broken or bad_anchor:
        print(f"\nLINK CHECK FAILED — {len(broken)} broken, "
              f"{len(bad_anchor)} bad anchor(s):")
        for rel, t in broken:
            print(f"  * {rel} -> {t}   (target does not exist)")
        for rel, t in bad_anchor:
            print(f"  * {rel} -> {t}   (no such heading in the target)")
        return 1
    print("LINK CHECK PASSED")
    return 0


if __name__ == "__main__":
    sys.exit(main())
