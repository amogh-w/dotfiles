#!/usr/bin/env bash
# Discover markdown files that function as steering/context docs for a coding
# agent, with line and word counts.
#
#   ./find_steering_docs.sh [ROOT]      # default: current directory
#
# Two tiers of output:
#   CERTAIN   -- filename alone identifies it (CLAUDE.md, AGENTS.md, ...)
#   PROBABLE  -- opening lines address an agent; confirm before trimming
#
# Excludes vendored trees and .git. Gitignored files are skipped except ones
# matching CERTAIN (CLAUDE.local.md is gitignored by design). A *.new file from
# the no-committed-version fallback never matches `.md$`, so it is not audited.
#
# --audit promotes the OTHER tier into scope. Trim mode touches steering docs
# only, because it deletes; audit mode checks every tracked doc for drift and
# deletes nothing, so the wider net is safe there and necessary -- the stalest
# claims found in practice sat in README.md, which trim mode excludes.
set -euo pipefail

AUDIT=0
ROOT="."
for a in "$@"; do
  case "$a" in
    --audit) AUDIT=1 ;;
    -h|--help) sed -n '2,18p' "$0"; exit 0 ;;
    *) ROOT="$a" ;;
  esac
done
cd "$ROOT"

# SKIP_NAMES is duplicated as SKIP_DIRS in trimstate.py, check_links.py and
# find_self_referential.py (bash array vs. Python set — can't share the literal
# across languages). Keep all four in sync by hand.
SKIP_NAMES=(.git node_modules .venv venv vendor target dist build __pycache__
            .next .tox site-packages)
PRUNE=(-name "${SKIP_NAMES[0]}")
for n in "${SKIP_NAMES[@]:1}"; do PRUNE+=(-o -name "$n"); done
SKIP_RE="(^|/)($(IFS='|'; echo "${SKIP_NAMES[*]}" | sed 's/\./\\./g'))/"

# Filename patterns that are steering docs by definition. Duplicated in
# ../scripts/trimstate.py as CERTAIN (bash regex vs. Python regex — can't
# share the literal). Keep both in sync by hand; both match case-insensitively.
CERTAIN_RE='(^|/)(CLAUDE(\.local)?\.md|AGENTS\.md|\.cursorrules|copilot-instructions\.md|GEMINI\.md)$'

# Phrases that mark a doc as written for an agent rather than a human reader.
AGENT_RE='instructions for (claude|the agent|coding agent)|notes for claude|when working (in|on) this|read this before|guidance (for|to) claude|for claude code|agent instructions|^you are '

count() { printf '%6s lines %7s words  %s\n' \
  "$(wc -l <"$1" | tr -d ' ')" "$(wc -w <"$1" | tr -d ' ')" "$1"; }

# Prefer git's own view of the tree: tracked plus untracked-but-not-ignored.
# Gitignored paths are almost never steering docs, and a repo that vendors
# downloaded data can hide hundreds of unrelated READMEs under it. The one
# exception is a CERTAIN filename: CLAUDE.local.md is gitignored by design, so
# ignored files matching CERTAIN_RE (outside vendored trees) are added back.
if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  mapfile -t ALL < <(
    { git ls-files --cached --others --exclude-standard
      git ls-files --others --ignored --exclude-standard \
        | grep -Ei "$CERTAIN_RE" | grep -Ev "$SKIP_RE"
    } 2>/dev/null \
    | grep -Ei '\.md$|(^|/)\.cursorrules$' | sort -u
  )
  SOURCE="git ls-files (gitignored paths excluded, except CERTAIN names)"
else
  mapfile -t ALL < <(
    find . \( "${PRUNE[@]}" \) -prune -o \
         -type f \( -name '*.md' -o -name '.cursorrules' \) -print 2>/dev/null \
    | sed 's|^\./||' | sort
  )
  SOURCE="find (not a git repo)"
fi

certain=(); probable=(); other=()
for f in "${ALL[@]}"; do
  [ -f "$f" ] || continue
  if printf '%s' "$f" | grep -Eiq "$CERTAIN_RE"; then
    certain+=("$f")
  elif head -c 2000 "$f" 2>/dev/null | grep -Eiq "$AGENT_RE"; then
    probable+=("$f")
  else
    other+=("$f")
  fi
done

MODE=$([ "$AUDIT" -eq 1 ] && echo audit || echo trim)
echo "Mode:   ${MODE}$([ "$AUDIT" -eq 1 ] && echo ' — every tracked doc in scope' || echo ' — steering docs only (--audit widens)')"
echo "Source: ${SOURCE}"
echo
echo "=== CERTAIN — steering docs by filename (${#certain[@]}) ==="
for f in "${certain[@]}"; do count "$f"; done
[ ${#certain[@]} -eq 0 ] && echo "  (none)"

echo
echo "=== PROBABLE — opening lines address an agent (${#probable[@]}) ==="
for f in "${probable[@]}"; do count "$f"; done
[ ${#probable[@]} -eq 0 ] && echo "  (none)"

echo
if [ "$AUDIT" -eq 1 ]; then
  echo "=== OTHER markdown — IN SCOPE (--audit) (${#other[@]}) ==="
else
  echo "=== OTHER markdown — out of scope unless asked (${#other[@]}) ==="
fi
# Slice the array rather than piping to `head`: under `set -o pipefail`, head
# closing the pipe early kills the whole script with SIGPIPE (exit 141), which
# only shows on repos with more than 40 files here -- so a small test fixture
# never reproduces it.
if [ ${#other[@]} -gt 0 ]; then
  for f in "${other[@]:0:40}"; do count "$f"; done
  [ ${#other[@]} -gt 40 ] && echo "  ... and $(( ${#other[@]} - 40 )) more"
else
  echo "  (none)"
fi

tot=0
scoped=("${certain[@]}" "${probable[@]}")
[ "$AUDIT" -eq 1 ] && scoped+=("${other[@]}")
for f in "${scoped[@]}"; do
  tot=$(( tot + $(wc -l <"$f" | tr -d ' ') ))
done
echo
echo "In-scope total (${MODE}): ${#scoped[@]} files, ${tot} lines"

# Name the files Phase 6 must treat specially: no committed HEAD version
# (untracked, gitignored, or staged but never committed) means an in-place
# write has nothing to recover from; dirty means this run's diff would mix with
# someone else's uncommitted work.
if git rev-parse --is-inside-work-tree >/dev/null 2>&1 && [ ${#scoped[@]} -gt 0 ]; then
  committed=$(git ls-tree -r --name-only HEAD -- "${scoped[@]}" 2>/dev/null || true)
  changed=$(git diff --name-only --relative HEAD -- "${scoped[@]}" 2>/dev/null || true)
  nohead=(); dirty=()
  for f in "${scoped[@]}"; do
    if ! grep -qxF -- "$f" <<<"$committed"; then nohead+=("$f")
    elif grep -qxF -- "$f" <<<"$changed"; then dirty+=("$f"); fi
  done
  if [ ${#nohead[@]} -gt 0 ]; then
    echo
    echo "NO COMMITTED VERSION (${#nohead[@]}) — write these to <file>.new, not in place:"
    for f in "${nohead[@]}"; do echo "    $f"; done
  fi
  if [ ${#dirty[@]} -gt 0 ]; then
    echo
    echo "UNCOMMITTED CHANGES (${#dirty[@]}) — commit or stash first, or this run's"
    echo "diff won't be attributable to the trim alone:"
    for f in "${dirty[@]}"; do echo "    $f"; done
  fi
fi
