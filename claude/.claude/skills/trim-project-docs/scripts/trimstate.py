#!/usr/bin/env python3
"""Idempotency and measurement state for trim-project-docs.

    trimstate.py status   [PATH ...]   # what drifted since the last trim
    trimstate.py report   [PATH ...]   # savings from this run's edits
    trimstate.py snapshot [--keep ...] [--verified ...] [--note ...] [PATH ...]

PATH is the in-scope list find_steering_docs.sh printed (CERTAIN + PROBABLE,
plus OTHER in audit mode). Pass it: the agent-detection heuristic lives in that
script only and is not re-derived here. With no PATH, the fallback is CERTAIN
filenames plus every file a previous snapshot recorded.

A doc with no committed version is written to `<doc>.new` instead of in place;
`report` diffs such a pair directly, and `snapshot` records the `.new` content.

State lives in `.trim-project-docs.json` at the repo root. It exists so a
periodic re-run concentrates on genuine drift instead of re-proposing cuts that
were already considered and rejected.

Recorded per file:

  hash      -- content hash at the last trim. A file whose hash is unchanged
               was not edited since, so its settled decisions still stand.

Recorded repo-wide (not per file -- the same line often recurs across docs):

  keeps     -- lines deliberately kept despite looking cuttable. `status`
               reports them as SETTLED so they are not re-litigated every run.
  verified  -- deferrals re-tested and found still true.

Token figures are an estimate (chars/4), adequate for reporting relative
savings and not presented as exact.
"""
import argparse
import hashlib
import json
import re
import subprocess
import sys
from datetime import date
from pathlib import Path

STATE_NAME = ".trim-project-docs.json"

# SKIP_DIRS is duplicated in find_steering_docs.sh (SKIP_NAMES), check_links.py
# and find_self_referential.py; CERTAIN is duplicated in find_steering_docs.sh
# (CERTAIN_RE). Can't share a literal across bash and Python -- keep all copies
# in sync by hand. Both CERTAIN copies match case-insensitively.
SKIP_DIRS = {".git", "node_modules", ".venv", "venv", "vendor", "target",
             "dist", "build", "__pycache__", ".next", ".tox", "site-packages"}
CERTAIN = re.compile(
    r"(^|/)(CLAUDE(\.local)?\.md|AGENTS\.md|\.cursorrules"
    r"|copilot-instructions\.md|GEMINI\.md)$", re.IGNORECASE)


def root() -> Path:
    cur = Path.cwd().resolve()
    for p in (cur, *cur.parents):
        if (p / ".git").exists() or (p / STATE_NAME).exists():
            return p
    return cur


def load(r: Path) -> dict:
    f = r / STATE_NAME
    if not f.exists():
        return {"version": 1, "runs": [], "files": {}}
    try:
        return json.loads(f.read_text())
    except json.JSONDecodeError as e:
        sys.exit(f"{f} is not valid JSON ({e}). Fix or delete it and re-run.")


def sha(text: str) -> str:
    return hashlib.sha256(text.encode("utf-8", "replace")).hexdigest()[:16]


def candidates(r: Path):
    """Tracked + untracked-not-ignored when in git, else a filtered walk.

    Gitignored paths are almost never steering docs, and a repo that vendors
    downloaded data can hide hundreds of unrelated files under one.
    """
    try:
        out = subprocess.run(
            ["git", "ls-files", "--cached", "--others", "--exclude-standard"],
            cwd=r, capture_output=True, text=True, timeout=30)
        if out.returncode == 0:
            return [r / line for line in out.stdout.splitlines() if line]
    except (OSError, subprocess.SubprocessError):
        pass
    # Not a git repo: the only path that needs SKIP_DIRS -- in git mode
    # .gitignore already keeps vendored trees out.
    return [p for p in r.rglob("*")
            if not any(x in SKIP_DIRS for x in p.relative_to(r).parts)]


def steering_docs(r: Path, st: dict, paths: list[str]) -> list[Path]:
    """The explicit in-scope list when given, else CERTAIN + previously recorded."""
    if paths:
        out = [Path(p).resolve() for p in paths]
        bad = [p for p, q in zip(paths, out)
               if not q.is_file() or not q.is_relative_to(r)]
        if bad:
            sys.exit(f"Not a file under {r}: {', '.join(bad)}")
        return sorted(set(out))
    out = {p for p in candidates(r)
           if p.is_file() and CERTAIN.search(p.relative_to(r).as_posix())}
    out |= {r / rel for rel in st.get("files", {}) if (r / rel).is_file()}
    return sorted(out)


def pending(p: Path) -> Path:
    """The `.new` sibling written when a doc has no committed version."""
    return p.with_name(p.name + ".new")


def in_git(r: Path) -> bool:
    try:
        return subprocess.run(["git", "rev-parse", "--is-inside-work-tree"],
                              cwd=r, capture_output=True).returncode == 0
    except OSError:
        return False


def git_show(r: Path, rel: str, ref: str = "HEAD") -> str | None:
    """Content of a committed file at a given ref, or None (no such version)."""
    out = subprocess.run(["git", "show", f"{ref}:{rel}"], cwd=r,
                         capture_output=True, text=True, timeout=30)
    return out.stdout if out.returncode == 0 else None


def measure(text: str) -> tuple[int, int, int]:
    """(lines, words, estimated tokens)"""
    return len(text.splitlines()), len(text.split()), len(text) // 4


def cmd_status(r: Path, st: dict, paths: list[str]) -> int:
    docs = steering_docs(r, st, paths)
    if not docs:
        print("No steering docs found.")
        return 0

    runs = st.get("runs", [])
    print(f"Repo: {r}")
    print(f"Last trim: {runs[-1]['date'] if runs else 'never'}"
          f"  ({len(runs)} run(s) recorded)\n")

    drifted, clean, fresh = [], [], []
    for p in docs:
        rel = p.relative_to(r).as_posix()
        rec = st["files"].get(rel)
        h = sha(p.read_text(errors="replace"))
        if rec is None:
            fresh.append(rel)
        elif rec.get("hash") != h:
            drifted.append(rel)
        else:
            clean.append(rel)

    def show(title, items):
        print(f"{title} ({len(items)})")
        for i in items:
            n = len((r / i).read_text(errors="replace").splitlines())
            print(f"    {n:>5} lines  {i}")
        if not items:
            print("    (none)")
        print()

    show("CHANGED since last trim — audit these", drifted)
    show("NEW — never trimmed", fresh)
    show("UNCHANGED since last trim — skip unless asked", clean)

    keeps = st.get("keeps", [])
    if keeps:
        print(f"SETTLED — {len(keeps)} line(s) previously marked KEEP. Do not")
        print("re-propose these cuts without a new reason:")
        for k in keeps[:10]:
            print(f"    ({k['since']}) {k['line'][:70]}")
        if len(keeps) > 10:
            print(f"    ... and {len(keeps) - 10} more")
        print()

    ver = st.get("verified", [])
    if ver:
        print(f"RE-TESTED — {len(ver)} deferral(s) confirmed still true. Re-test")
        print("only if the thing being deferred has since been built:")
        for v in ver[:10]:
            print(f"    ({v['since']}) {v['line'][:70]}")
        if len(ver) > 10:
            print(f"    ... and {len(ver) - 10} more")
        print()

    new = [p for p in docs if pending(p).is_file()]
    if new:
        print(f"UNAPPLIED — {len(new)} <doc>.new file(s) from a previous run. "
              "Apply or delete them\nbefore trimming again:")
        for p in new:
            print(f"    {pending(p).relative_to(r).as_posix()}")
        print()

    return 0


def cmd_report(r: Path, st: dict, paths: list[str]) -> int:
    """Savings per doc. "Before" is git's HEAD for an in-place edit, or the
    original itself when a `<doc>.new` sibling holds the rewrite."""
    git = in_git(r)
    pairs, untracked, news = [], [], []
    for p in steering_docs(r, st, paths):
        rel = p.relative_to(r).as_posix()
        if pending(p).is_file():  # no-committed-version fallback
            news.append(rel + ".new")
            pairs.append((rel + " (.new)", p.read_text(errors="replace"),
                          pending(p).read_text(errors="replace")))
            continue
        before = git_show(r, rel) if git else None
        if before is None:
            if git:
                untracked.append(rel)
            continue
        after = p.read_text(errors="replace")
        if before == after:
            continue  # unchanged since HEAD — nothing to report
        pairs.append((rel, before, after))

    if not pairs and not untracked:
        print("No changes in any in-scope doc — nothing to report.")
        print("(Run this after trimming, before committing.)")
        return 0

    print(f"{'file':<44}{'lines':>14}{'words':>14}{'~tokens':>13}")
    print(f"{'':<44}{'before→after':>14}{'before→after':>14}{'saved':>13}")
    print("-" * 85)

    tl = tw = tt = 0
    for rel, before, after in pairs:
        lb, wb, tb = measure(before)
        la, wa, ta = measure(after)
        tl += lb - la
        tw += wb - wa
        tt += tb - ta
        shown = rel if len(rel) <= 43 else "…" + rel[-42:]
        print(f"{shown:<44}{f'{lb}→{la}':>14}{f'{wb}→{wa}':>14}{tb - ta:>13}")
    for rel in untracked:
        shown = rel if len(rel) <= 43 else "…" + rel[-42:]
        print(f"{shown:<44}{'(untracked, no prior version)':>41}")

    print("-" * 85)
    print(f"{f'TOTAL ({len(pairs)} files)':<44}{tl:>14}{tw:>14}{tt:>13}")
    print("\n~tokens is chars/4 — an estimate for comparison, not an exact count.")
    if git:
        print("\nReview in-place edits with `git diff`, then commit when satisfied.")
    if news:
        print("\nRewrites held in .new files (no committed version to diff against).")
        print("Review with `diff -u <doc> <doc>.new`; apply with `mv <doc>.new <doc>`:")
        for n in news:
            print(f"    {n}")
    return 0


def _merge(existing: list, lines: list[str]) -> list:
    """Repo-scoped, deduplicated by content hash, newest wording wins."""
    out = {e["hash"]: e for e in existing}
    for raw in lines:
        line = raw.strip()
        out[sha(line)] = {"hash": sha(line), "line": line[:200],
                          "since": date.today().isoformat()}
    return sorted(out.values(), key=lambda e: e["hash"])


def cmd_snapshot(r: Path, st: dict, keeps: list[str], verified: list[str],
                 note: str, paths: list[str]) -> int:
    docs = steering_docs(r, st, paths)
    if not docs:
        print("No steering docs found — nothing to snapshot.")
        return 0

    # Both lists are repo-scoped, not per file: the same line often recurs
    # across docs, and attaching a copy to every file made the settled count sum
    # to files x decisions instead of the number of distinct decisions.
    #
    # They mean different things and are kept apart deliberately:
    #   keeps    -- "do not propose cutting this again"
    #   verified -- "this deferral was RE-TESTED on that date and still held"
    # A verified entry expires in a way a keep does not: if the thing it defers
    # gets built, the line becomes OVERTAKEN and must be re-examined.
    st["keeps"] = _merge(st.get("keeps", []), keeps)
    st["verified"] = _merge(st.get("verified", []), verified)

    for p in docs:
        rel = p.relative_to(r).as_posix()
        # A pending .new holds the post-trim content; hash that, so the record
        # matches once the user applies it.
        text = (pending(p) if pending(p).is_file() else p).read_text(
            errors="replace")
        lines, words, toks = measure(text)
        st["files"][rel] = {"hash": sha(text), "lines": lines, "words": words,
                            "tokens_est": toks}

    st.setdefault("runs", []).append(
        {"date": date.today().isoformat(), "files": len(docs), "note": note})
    (r / STATE_NAME).write_text(json.dumps(st, indent=2) + "\n")

    print(f"Snapshotted {len(docs)} file(s) to {STATE_NAME}")
    if keeps:
        print(f"Recorded {len(keeps)} KEEP decision(s); "
              f"{len(st['keeps'])} settled in total.")
    if verified:
        print(f"Recorded {len(verified)} deferral(s) re-tested and still true; "
              f"{len(st['verified'])} tracked in total.")
    ignored = subprocess.run(["git", "check-ignore", "-q", STATE_NAME],
                             cwd=r, capture_output=True).returncode == 0
    if ignored:
        print(f"{STATE_NAME} is gitignored — the settled decisions stay on this "
              f"machine and do not reach anyone else's re-run.")
    else:
        print("Commit this file to share what was settled, or gitignore it to "
              "keep the run state local.")
    return 0


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = ap.add_subparsers(dest="cmd", required=True)
    scope = argparse.ArgumentParser(add_help=False)
    scope.add_argument("paths", nargs="*", metavar="PATH",
                       help="in-scope docs from find_steering_docs.sh")
    sub.add_parser("status", parents=[scope],
                   help="what drifted since the last trim")
    sub.add_parser("report", parents=[scope],
                   help="savings from this run's edits")
    sp = sub.add_parser("snapshot", parents=[scope], help="record this run")
    sp.add_argument("--keep", action="append", default=[], metavar="LINE",
                    help="a line kept despite looking cuttable (repeatable)")
    sp.add_argument("--verified", action="append", default=[], metavar="LINE",
                    help="a deferral re-tested and still true (repeatable)")
    sp.add_argument("--note", default="", help="one-line note for the run log")
    a = ap.parse_args()

    r = root()
    st = load(r)
    if a.cmd == "status":
        return cmd_status(r, st, a.paths)
    if a.cmd == "report":
        return cmd_report(r, st, a.paths)
    return cmd_snapshot(r, st, a.keep, a.verified, a.note, a.paths)


if __name__ == "__main__":
    sys.exit(main())
