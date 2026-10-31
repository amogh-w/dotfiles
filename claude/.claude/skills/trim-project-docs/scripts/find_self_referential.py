#!/usr/bin/env python3
"""Find prose where a doc narrates its own revision history.

    find_self_referential.py [ROOT]      # default: current directory
    find_self_referential.py --quiet     # only print findings
    find_self_referential.py --strict    # exit non-zero on markdown hits

WHY THIS EXISTS
---------------
A doc revised by an agent that has just corrected itself accumulates
autobiography: "an earlier version of this file claimed the opposite", "why this
was wrong for two days", "corrected 2026-10-01". The finding underneath is
usually worth keeping. The confession is not -- it is dead weight to every
future reader, it dates the document, and in a work product that leaves the team
it reads as churn rather than engineering.

This is `SELF-REFERENTIAL`, flag 8 in references/classification.md. It is not
`NARRATIVE`: narrative is history of the SYSTEM, this is history of the WRITING.

WHAT THIS IS NOT
----------------
A code comment explaining why an implementation is shaped the way it is is a
KEEPER, even when it opens "an earlier version":

    # An earlier version called snapshot_download() with no patterns and pulled
    # 818 GB for 12 German hours. Always pass hf_allow_patterns.

That stops someone reverting to the broken approach, which is the keeper test
exactly. So hits in code files are reported SEPARATELY and never fail the run --
they need a human decision, and most of them stay. Only markdown prose is
treated as a finding.

Provenance in a compliance or audit record also stays: "licence corrected
2026-09-09, source re-fetched" is data lineage, not autobiography.
"""
import argparse
import re
import subprocess
import sys
from pathlib import Path

# SKIP_DIRS is duplicated in find_steering_docs.sh (SKIP_NAMES), trimstate.py
# and check_links.py -- keep all four in sync by hand.
SKIP_DIRS = {".git", "node_modules", ".venv", "venv", "vendor", "target",
             "dist", "build", "__pycache__", ".next", ".tox", "site-packages"}
CODE_SUFFIXES = {".py", ".sh", ".js", ".ts", ".go", ".rs", ".rb", ".java",
                 ".c", ".h", ".cpp", ".yaml", ".yml", ".toml"}

# The tells. Deliberately anchored on the DOCUMENT as subject -- "an earlier
# version of this file/guide/line", not a bare "earlier" -- so ordinary prose
# about earlier experiments does not fire.
TELLS = [
    (r"an earlier (?:version|pass|draft|probe) of (?:this|the) "
     r"(?:file|guide|line|page|doc|report|probe|section)", "earlier version of this doc"),
    (r"\bthis (?:file|guide|page|doc|section|line) (?:previously|used to|once) "
     r"(?:said|claimed|recorded|asserted|listed)", "this doc previously claimed"),
    (r"\b(?:previously|earlier) claimed the opposite", "previously claimed the opposite"),
    # Narrow on purpose. A first version matched any heading containing
    # "correct" and fired on "### Stage 3, corrected" -- a run log describing the
    # fixed command, which is exactly the content to keep. The doc-revision
    # sense is a heading that OPENS with "Correction", or one stamped with a
    # correction date.
    (r"^#+\s*correction\b", "'Correction' heading"),
    (r"^#+.*\bcorrected \d{4}-\d{2}-\d{2}", "heading stamped with a correction date"),
    (r"^#+.*\bwhy this was wrong\b", "'why this was wrong' heading"),
    (r"\bthis (?:invalidat|supersed|correct)(?:es|ed) an earlier", "invalidates an earlier claim"),
    (r"\bI (?:did not|didn't|failed to) (?:check|verify|test|notice)", "first-person admission"),
    (r"\bmy (?:first|earlier) (?:read|take|reading|conclusion)", "first-person admission"),
    (r"\bwas wrong for (?:two |three |several |\d+ )?(?:days|weeks|hours)", "duration of being wrong"),
    (r"\bthe old (?:claim|entry|reading|figure) (?:named|said|was)", "referring to the old text"),
]
COMPILED = [(re.compile(p, re.I | re.M), label) for p, label in TELLS]

FENCE = re.compile(r"^\s*(```|~~~)")


def tracked(root: Path):
    """Tracked + untracked-but-not-ignored files when in git (the same source
    as check_links.py and trimstate.py), else a walk skipping SKIP_DIRS."""
    try:
        out = subprocess.run(["git", "-C", str(root), "ls-files", "--cached",
                              "--others", "--exclude-standard"],
                             capture_output=True, text=True, check=True).stdout
        paths = [root / p for p in out.splitlines() if p.strip()]
        if paths:
            return [p for p in paths if p.is_file()]
    except (subprocess.CalledProcessError, FileNotFoundError):
        pass
    return [p for p in root.rglob("*")
            if p.is_file() and not SKIP_DIRS & set(p.parts)]


def outside_fences(text: str):
    """(line number, line) for lines not inside a fenced code block.

    An example of the anti-pattern, quoted inside a fence in the skill's own
    reference docs, must not be reported as an instance of it.
    """
    inside = False
    for n, line in enumerate(text.splitlines(), 1):
        if FENCE.match(line):
            inside = not inside
            continue
        if not inside:
            yield n, line


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("root", nargs="?", default=".")
    ap.add_argument("--quiet", action="store_true")
    ap.add_argument("--strict", action="store_true",
                    help="exit non-zero on markdown findings (default: report only)")
    args = ap.parse_args()
    root = Path(args.root).resolve()

    prose, code = [], []
    files = tracked(root)
    for path in files:
        if path.suffix not in {".md", ".markdown"} | CODE_SUFFIXES:
            continue
        try:
            text = path.read_text(encoding="utf-8")
        except (UnicodeDecodeError, OSError):
            continue
        is_md = path.suffix in {".md", ".markdown"}
        for n, line in outside_fences(text) if is_md else enumerate(text.splitlines(), 1):
            for rx, label in COMPILED:
                if rx.search(line):
                    rel = path.relative_to(root)
                    (prose if is_md else code).append(
                        (f"{rel}:{n}", label, line.strip()[:90]))
                    break

    if not args.quiet:
        print(f">> scanned {len(files)} file(s) under {root}\n")

    if prose:
        print(f"SELF-REFERENTIAL in prose -- {len(prose)} finding(s). "
              f"Reframe forward; keep the trap, drop the autobiography:\n")
        for loc, label, snippet in prose:
            print(f"  {loc}\n      [{label}] {snippet}")
        print()
    elif not args.quiet:
        print("  no revision-history noise in markdown prose\n")

    if code:
        print(f"IN CODE -- {len(code)} hit(s). NOT findings by default: a comment "
              f"explaining\nwhy an implementation is shaped a certain way is a "
              f"KEEPER. Decide per hit.\n")
        for loc, label, snippet in code:
            print(f"  {loc}\n      [{label}] {snippet}")
        print()

    if args.strict and prose:
        print("SELF-REFERENTIAL CHECK FAILED")
        return 1
    print("SELF-REFERENTIAL CHECK PASSED" if not prose else
          "SELF-REFERENTIAL: findings above are advisory (pass --strict to gate)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
