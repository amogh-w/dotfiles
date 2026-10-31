---
name: trim-project-docs
description: This skill should be used when the user asks to "trim project docs", "audit my CLAUDE.md", "clean up steering files", "my CLAUDE.md is too long", "de-duplicate AGENTS.md", "prune context docs", "check my docs for drift", "are my docs stale", "do my docs still match the code", "check for broken links in my docs", or wants markdown steering/context documentation for coding agents reviewed for staleness, duplication, contradictions, and content recoverable by reading the code. Runs in one of two modes — trim (deletes, steering docs only) or audit (repo-wide drift check). Plans in plan mode first, writes only after an explicit mode switch.
version: 0.3.0
---

# Trim Project Docs

Audit and trim the markdown files that steer a coding agent — `CLAUDE.md`,
`AGENTS.md`, and per-directory context docs — so they carry only what cannot be
recovered by reading the code.

Steering docs decay in a specific way: they accumulate narrative, restate what
functions already say, and describe files that were deleted two refactors ago.
Every stale line costs context on every session and, worse, actively misleads.
This skill finds that content, proposes a tighter rewrite, and measures what was
removed.

**Two hard rules govern this skill and override any conflicting instinct:**

1. **No file is written while plan mode is active** — see "Mode discipline".
2. **Non-recoverable knowledge is never deleted** — see "The keeper test".

## Pick a mode first

| | **trim** | **audit** |
|---|---|---|
| Asked as | "trim the docs", "CLAUDE.md is too long", "de-duplicate" | "check for drift", "are my docs stale", "do the docs match the code" |
| Scope | steering docs only | every tracked doc, README and all |
| Flags applied | all nine | `CONTRADICTION` `PHANTOM` `CROSSREF` `OVERTAKEN` `SELF-REFERENTIAL`, plus links and counts |
| Deletes | yes — that is the point | no; corrects claims where they stand |
| Discovery | `find_steering_docs.sh` | `find_steering_docs.sh --audit` |

Both modes run the same seven phases. State which mode is running before Phase 1;
when the request is ambiguous, ask rather than assume, because one deletes.

Audit is the cheaper and more frequent job: drift accrues continuously, whereas
a doc only needs trimming once it has grown. In practice the stalest claims sit
in files trim mode excludes — one sweep found 22 dead dates in a root `README.md`
that the trim scope never looked at.

## Workflow

### Phase 0 — Enter plan mode

Enter plan mode before reading anything, using `EnterPlanMode` if the session is
not already in it. Analysis happens entirely in plan mode. If plan mode cannot
be entered, say so and stop rather than proceeding to edits.

### Phase 1 — Discover the steering docs

Run `scripts/find_steering_docs.sh` from the repo root. It lists candidates with
line and word counts, excludes vendored trees and `.git`, and names two kinds of
in-scope file that Phase 6 handles differently: **no committed version**
(untracked, gitignored, or never committed) and **uncommitted changes**. Keep
the full in-scope list it prints; Phase 7 passes it to `trimstate.py`.

In scope by default:
- `CLAUDE.md` / `CLAUDE.local.md` at any depth (`CLAUDE.local.md` is found even
  when gitignored, as it usually is)
- `AGENTS.md`, `GEMINI.md`, `.cursorrules`, `.github/copilot-instructions.md`
- `.claude/` docs, and any markdown whose opening lines address an agent
  ("instructions for Claude", "when working in this directory", "read this
  before")

Out of scope unless the user asks: `README.md` aimed at humans, changelogs,
blog posts, ADRs, licence files, upstream vendored docs.

`README.md` is the ambiguous case. Include it only when it carries agent-facing
convention text (build commands, "always run X before Y"); treat prose written
for human newcomers as out of scope. State the call made for each README.

### Phase 2 — Classify every line

Run the two mechanical checks first — they are deterministic and cost nothing:

```bash
python3 scripts/check_links.py           # every link and heading anchor
python3 scripts/find_self_referential.py # revision-history noise
```

Then reproduce every **count** a doc asserts ("27 repos", "44 leaves") against
the data it claims to summarise. Prose numbers drift silently because nothing
recomputes them.

Then read each in-scope file in full, and consult
**`references/classification.md`** for the nine flag categories, their tests,
and their cut/keep examples:

| Flag | What it catches |
|---|---|
| `RECOVERABLE` | Restates what the code plainly says |
| `NARRATIVE` | History or rationale that no longer changes behaviour |
| `CONTRADICTION` | Disagrees with another doc or with the code |
| `PHANTOM` | Names a file, symbol, command, or flag that no longer exists |
| `DUPLICATE` | Same convention stated in several files |
| `OVERTAKEN` | A deferral or TODO the world has moved past |
| `CROSSREF` | Asserts another file's *state*, which has since changed |
| `SELF-REFERENTIAL` | The doc narrating its own revision history |
| `KEEP` | Non-recoverable — see the keeper test below |

Five of these require verification, not judgement:

- `PHANTOM` — confirm every path, command, function, flag and config key a doc
  names actually exists. Never flag on suspicion; grep for it, and resolve a
  path against every plausible root before calling it missing.
- `CONTRADICTION` — establish which side is true from the code, not from which
  doc reads more confidently.
- `OVERTAKEN` — **re-run the claim.** Most deferrals survive this; report the
  ratio of tested to genuinely stale.
- `CROSSREF` — read the other file.
- `SELF-REFERENTIAL` — decide whether the sentence is about the *document's*
  wording or the *code's* behaviour. A comment explaining why an implementation
  is shaped a certain way is a KEEPER even when it opens "an earlier version";
  a doc narrating its own correction is not.

**`references/classification.md` ends with a false-positive discipline section.
Read it before reporting anything.** Four traps recur: paths stated relative to
a package rather than a repo root, counts whose unit is unstated, regexes taking
the first match where the last is current, and unanchored patterns matching
inside longer words. A check that yields more false positives than findings is
a broken check — fix it before reporting from it.

### Phase 3 — Draft the rewrites

For each flagged file, write the full replacement text, not a description of it:

- Convert prose to terse imperative bullets. "We decided that contributors
  should always run the formatter before committing, because CI will otherwise
  fail" becomes "Run `fmt` before committing; CI fails otherwise."
- Delete `RECOVERABLE` and `NARRATIVE` content outright.
- Resolve `CONTRADICTION` by checking the code and keeping the true statement;
  if the code is ambiguous, keep both with a `TODO:` naming the conflict rather
  than silently picking one.
- Delete `PHANTOM` lines.
- Hoist `DUPLICATE` conventions to the root file and remove the copies.
- Close an `OVERTAKEN` TODO **by citing the source that answered it**, never by
  asserting the answer. If no such record exists, the TODO stays.
- Delete an `OVERTAKEN` note about a now-fixed defect, keeping the durable fact.
- Repoint a `CROSSREF` at what is true now, or delete it. Prefer describing the
  requirement over the other file's current state.
- **Reframe `SELF-REFERENTIAL` forward; do not just delete it.** "An earlier
  version said X, which was wrong" usually wraps a real trap. Keep the trap,
  drop the autobiography: "X looks true because Y, and here is what distinguishes
  them." The measurement, the warning and the instruction survive; the first
  person and the timeline go.
- Carry every `KEEP` through verbatim unless it can be shortened without losing
  the failure mode it names.

Preserve heading structure where it still fits the content. Do not invent
sections the original did not have.

### Phase 4 — Recommend a tier structure

Consult **`references/tiering.md`**. In short: project-wide conventions, build
and test commands, and the architecture overview belong in the root file;
subsystem rules that only matter inside one directory belong in a nested file
there, where they load only in that context. Propose specific moves, naming
source and destination.

### Phase 4b — Propose a gate when a class recurs

**When a finding class is machine-checkable and has three or more instances,
propose a repo-specific gate script alongside the fixes.** This is usually the
most valuable thing a run produces: fixing turns a class false once, a gate
turns it unable to recur.

The case that proves it: one repo stored a verification date in a manifest
field, in 25 guide files, and in a root README table. The guide copies and the
README copies had drifted **independently** from the same source — 23 and 22
lines wrong respectively. Syncing 45 lines fixed a day; a 90-line
`check_guides.py` fixed the class.

Rules for the gate:

- Name it for what it guards (`check_guides.py`, not `validate.py`).
- Model it on any check the repo already has, so it fits the local idiom.
- Cover only classes with a machine-readable source of truth. Prose judgement
  does not gate.
- **Prove it fails.** Break one instance, watch it exit non-zero, restore. A
  gate reported as working without that is an assumption.
- State how to run it, and offer it for CI or a pre-commit hook.

Do not propose a gate for a one-off. Three instances is the floor because below
that the script costs more than the drift.

### Phase 5 — Present the plan and hand back control

Present via `ExitPlanMode`. The plan must contain:

- Every file to be touched, with current line/word count
- Per file: what is removed, merged, moved, or kept, with counts per flag
- At least two before/after excerpts showing real content from this repo
- The tier-structure moves
- Projected savings in lines and approximate tokens
- Any `KEEP` lines that looked cuttable but were retained, with the reason

End the plan with this line verbatim:

> **Switch out of plan mode into auto/act mode before approving. No file will be
> written until the mode has actually changed — approval alone is not enough.**

### Phase 6 — Write (only after the mode has genuinely changed)

Default output is **in place**, directly via `Edit`/`Write`. Git is the record
of what changed: a file with a committed `HEAD` version gets a reviewable
`git diff` and a `git restore` undo for free.

That default needs a committed version to fall back on. **For any file without
one, write to `<original>.new` beside it instead** and leave the original
untouched. This covers:

- a repo with no `.git` at all (`git rev-parse --is-inside-work-tree` fails)
- an untracked file, a gitignored one such as `CLAUDE.local.md`, or one staged
  but never committed — Phase 1 lists these under "NO COMMITTED VERSION"

Say why for each such file: an in-place edit there would destroy the only copy,
because there is no `git show HEAD:<file>` to recover it from.

An in-place write also assumes the file is clean beforehand — Phase 1 lists any
with "UNCOMMITTED CHANGES". Writing on top of one would mix this run's edits
into someone else's uncommitted work, and `git diff` could no longer tell the
two apart. If a file comes in dirty, say so and ask whether to proceed, stash
first, or skip that file.

Do not snapshot yet — Phase 7's sweep may edit more files. Do keep track of the
lines Phase 2/3 kept despite looking cuttable and the deferrals re-tested and
found still true; Phase 7 records them.

### Phase 7 — Sweep back-references, then report

**Before reporting anything, grep the repo for references to what was just
deleted.** A deletion pass is what creates `CROSSREF` breakage, and it is the
pass least likely to look for it. Search for removed section headings, removed
file names, and any distinctive phrase from the deleted text — then re-run
`check_links.py`.

This is not optional. One trim removed a 639-line section and left three other
documents describing it in the present tense; all three were found later by a
separate sweep that should not have been needed.

Then produce a summary table: file, lines before, lines after, lines removed,
approximate tokens saved, and a one-line note on what was merged or deleted.
Total the savings. List any file examined and left unchanged, so a re-run has a
record of what was already clean.

Report three things that are easy to omit and make the rest credible:

- **candidates checked and discarded**, with the reason — the false positives
- **deferrals re-tested and still true**, which are findings, not omissions
- **`KEEP` lines that looked cuttable**, with why they stayed

Run `scripts/trimstate.py report <in-scope paths>` for the before/after
savings table, passing the full list Phase 1 printed. For an in-place edit it
diffs the committed `HEAD` version against the working tree (why Phase 1 needs
the file clean beforehand); for a `.new` file it diffs the original against the
`.new`; an untracked doc with no `.new` shows as "(untracked, no prior
version)".

Point the user at `git diff` to review in-place edits. **List every `.new` file
by path in the summary**, with `diff -u <doc> <doc>.new` to review and
`mv <doc>.new <doc>` to apply. Applying, `git add` and `git commit` are the
user's to run, not this skill's.

Last, once the sweep's edits are done, record the run:

```bash
python3 scripts/trimstate.py snapshot \
  --keep "<line kept despite looking cuttable>" \
  --verified "<deferral re-tested and still true>" \
  --note "<one-line summary>" <in-scope paths>
```

Pass one `--keep` per `KEEP` line reported above and one `--verified` per
deferral re-tested in Phase 2/3. These flags are what stop the next run from
re-proposing the same cuts — a snapshot without them records hashes only.

## Mode discipline

Plan mode exists here because this skill deletes things. The failure to avoid is
writing on the strength of an approving phrase while still in plan mode.

- Never call `Write` or `Edit` while plan mode is active, whatever the user says.
  "Looks good", "yes do it", and "approved" are not mode switches.
- If a write is refused, stop. Do not retry, and do not route around it with a
  shell command. Report that the mode has not changed and wait.
- The plan itself must carry the mode-switch instruction from Phase 5 verbatim,
  so the requirement is visible in the approval UI rather than only in chat.

## The keeper test

Before deleting any line, ask: **could a competent engineer with full repo access
derive this by reading the code in about two minutes?**

If no, it stays — even at full-sentence length, even if it reads like narrative.

Always keep:
- Failure modes and their causes — "don't use library X here, it breaks under Y"
- Version pins with the reason for the pin
- Environment and toolchain gotchas that no file records
- External constraints: licence terms, API quotas, security and legal rules
- Non-obvious ordering requirements — "run A before B or the cache goes stale"
- Anything whose removal would let someone reintroduce a fixed bug

The tell is a *because* that names a real failure. "We use pytest because it's
nice" is narrative. "Use pytest, not unittest — the async fixtures deadlock under
unittest" is a keeper. When genuinely uncertain, keep it and note the
uncertainty in the report.

## Idempotency

Safe to re-run on a schedule; docs drift again.

- Writes land directly in the tracked file, so a second run's discovery pass
  sees ordinary file content. The one leftover is a `<doc>.new` for a file with
  no committed version; discovery never matches it, and `trimstate.py status`
  lists any still unapplied.
- `scripts/trimstate.py snapshot` records a content hash per file plus the
  keeper decisions made. `scripts/trimstate.py status` then reports which files
  changed since the last trim, so a re-run concentrates on drift instead of
  re-litigating settled calls.
- Re-flagging a line previously marked `KEEP` requires a reason that the state
  file does not already answer. Do not propose the same cut twice.
- `snapshot --verified "<line>"` records a deferral re-tested and found still
  true. `status` lists these separately from `KEEP`, because they expire
  differently: a keeper stays a keeper, whereas a verified deferral becomes
  `OVERTAKEN` the moment the thing it defers gets built. Re-test one only when
  that has happened.
- Files already terse are reported as unchanged rather than rewritten for the
  sake of it. A trim that removes nothing is a valid outcome; say so plainly.

## Additional resources

- **`references/classification.md`** — the nine flags with their tests and
  cut/keep examples, ending in the false-positive discipline section
- **`references/tiering.md`** — root vs nested placement, with a worked layout
- **`scripts/find_steering_docs.sh`** — discovery; `--audit` widens scope from
  steering docs to every tracked doc. Its CERTAIN filename pattern is
  duplicated in `trimstate.py`, and its skip-directory list in all three Python
  scripts (can't share a literal across bash and Python) — update every copy
  together if either ever changes.
- **`scripts/check_links.py`** — every markdown link and heading anchor; exits
  non-zero on a real break, skips template scaffolding
- **`scripts/find_self_referential.py`** — prose where a doc narrates its own
  revision history. Advisory by default (`--strict` to gate); code hits are
  listed separately because a comment explaining an implementation is a keeper
- **`scripts/trimstate.py`** — `snapshot` / `status` / `report` for idempotent
  re-runs and savings measurement; takes the in-scope paths from discovery, and
  `--keep` / `--verified` record decisions
