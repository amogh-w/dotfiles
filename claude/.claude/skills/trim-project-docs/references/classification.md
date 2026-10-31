# Classification reference

The nine flags, with the test that assigns each and worked cut/keep pairs.

Apply flags per *claim*, not per file. A single paragraph often contains one
keeper sentence buried in four narrative ones; the trimmed version keeps the one.

---

## 1. RECOVERABLE — restates what the code says

**Test:** open the file the line describes. If the line adds nothing the code
does not already state at a glance, cut it.

This is usually the largest category by volume and the safest to remove, because
the code is the source of truth and cannot drift from itself.

**Cut:**
```markdown
The `UserService` class has methods `create()`, `update()`, and `delete()`.
`create()` takes a name and an email and returns a User object.
The `utils/` directory contains utility functions.
`config.py` holds configuration.
Routes are defined in `routes.ts`.
```

**Keep** — these look similar but carry non-recoverable facts:
```markdown
`UserService.delete()` soft-deletes; hard deletion is the nightly job only.
`utils/retry.py` is load-bearing for the payment path — changes need a soak test.
```

The keeper in each case is a constraint or consequence, not a description.

**Edge case:** an architecture overview that orients a reader — "requests enter
at `api/`, fan out through `workers/`, land in `store/`" — is not recoverable at
a glance from any single file. Keep a short one in the root doc. Cut it when it
degenerates into a directory listing.

---

## 2. NARRATIVE — history or rationale with no present-day effect

**Test:** if this were deleted, would anyone behave differently tomorrow? If no,
cut it.

Past tense and migration language are the tells: "originally", "we used to",
"previously", "was migrated from", "the old system".

**Cut:**
```markdown
We originally used Redux but migrated to Zustand in 2023 because the team found
the boilerplate excessive. The migration took two sprints.

This module was written during the Q2 push and has been refactored twice since.

We debated using gRPC here and decided against it.
```

**Keep** — history that still governs behaviour:
```markdown
Legacy `v1/` endpoints stay until the mobile app drops support — do not delete.

Redux remains in `admin/` only; new state goes in Zustand. Do not mix them in
one component tree.
```

The difference: the keeper tells someone what to do today. The cut only explains
how things came to be.

**Do not over-apply.** Rationale that prevents a bad change is a keeper, even
though it reads like history. "Do not switch back to X, it deadlocks under Y" is
a failure mode wearing narrative clothing.

---

## 3. CONTRADICTION — disagrees with another doc or with the code

**Test:** mechanical. Compare claims across files, and compare every claim
against the tree.

**Verify, do not assume.** Before flagging, confirm which side is true:

```bash
# doc says "run `npm test`" — does that script exist?
grep -n '"test"' package.json

# doc says "Python 3.11" — what do the configs pin?
grep -rn 'python_requires\|requires-python\|python-version' \
  setup.py pyproject.toml .github/workflows/ 2>/dev/null
```

**Common shapes:**
- Root doc says 4-space indent, a nested doc says 2
- One doc says "never commit directly", another documents a direct-commit flow
- A doc pins a version the lockfile contradicts
- Two docs name different entry points for the same service

**Resolution order:**
1. The code wins over any doc.
2. Where the code is genuinely ambiguous, keep both statements and add a
   `TODO:` naming the conflict.
3. Never silently pick a side. A quietly resolved contradiction is how a wrong
   convention becomes canon.

Report contradictions even when they fall outside the files being trimmed — a
conflict between two docs is worth surfacing regardless.

---

## 4. PHANTOM — names something that no longer exists

**Test:** grep for it. No hits, and no plausible rename, means cut.

This flag requires evidence. A phantom claimed on suspicion is how real
instructions get deleted.

```bash
# every backticked path in a doc — does it exist?
grep -o '`[a-zA-Z0-9_./-]\+`' CLAUDE.md | tr -d '`' | sort -u | \
  while read -r p; do [ -e "$p" ] || echo "MISSING: $p"; done

# a named symbol
rg -n 'legacy_sync_handler' --type py
```

**Cut:**
```markdown
See `scripts/deploy_old.sh` for the deploy steps.   # file deleted
Set `ENABLE_BETA=1` to turn on the beta path.       # flag removed from code
Tests live in `spec/`.                              # now `tests/`
```

**Before cutting, check for a rename.** `scripts/deploy_old.sh` →
`scripts/deploy.sh` means the line should be corrected, not deleted. Check
`git log --diff-filter=D --name-only` and `git log --follow` when unsure.

A phantom that documents a *deliberate* absence is a keeper: "there is no
`make test` — run `pytest` directly, the Makefile target was removed because it
masked failures."

---

## 5. DUPLICATE — same convention in several files

**Test:** does this rule apply repo-wide? If yes, it belongs in the root file
once.

**Cut from nested files, keep in root:**
```markdown
# repeated verbatim in six nested CLAUDE.md files
Use 4-space indentation.
Run the formatter before committing.
Never commit secrets.
```

**Keep in the nested file** — scoped rules that genuinely differ from the root:
```markdown
# in frontend/CLAUDE.md
This directory is 2-space, unlike the repo default — the linter enforces it.
```

An exception to a root rule is not a duplicate. It is a keeper, and it reads
more clearly when it names the rule it overrides.

**Near-duplicates matter too.** Three files saying the same thing in three
phrasings is worse than verbatim repetition: it invites the reader to hunt for a
distinction that is not there. Collapse to one wording.

---

## 6. OVERTAKEN — a deferral or warning that events have passed

**Test: re-run the claim.** A `TODO:` is not stale because it is old. It is stale
because the thing it describes has changed.

**Most deferrals survive this test, and saying so is the point.** A real audit:
13 lines matched deferral language; 9 were false positives on "ap*pending*",
"de*pending*" and "s*pending*"; two were re-verified as still true (the deferred
scorer still did not exist; the manual pass still had 112 unjudged cells against
1 recorded verdict); exactly **one** was genuinely overtaken. Report the ratio.

**Two shapes, two resolutions:**

*A question that got answered* — close it **citing the answer's source**, never
by asserting the answer yourself:

```markdown
- TODO: license text has genuinely conflicting sources. Get the actual full
  license from the distributor before relying on either characterization.
```
became, only because an audit file already held the answer with a URL and a
verbatim quote:
```markdown
- **Licence: META-SHARE ResearchUsageOnly-NoRedistribution — not commercially
  usable.** Audited 2026-09-10 against the distributor's own page, which states:
  *"…available under the META-SHARE ResearchUsageOnly NoRedistribution
  License."* Recorded with that source in `../licenses.yaml`.
```
Had no such record existed, the TODO stays. Closing a question by answering it
from memory is how a guess becomes canon.

*A note about a defect now fixed* — delete the note, keep the substance:

```markdown
(`run_eval.py:24-28` still says 8 and lists Orpheus — fix that when the pass
is built.)                                        # the defect was fixed an hour later
```
→ deleted; the durable fact ("only 2 of the 7 leaves expose a multi-turn CLI")
already sat in the same paragraph.

**Do not confuse `OVERTAKEN` with tidying.** A deferral that still holds is
load-bearing: it tells a reader why something is missing, which stops them
concluding it was forgotten. Removing a true deferral is a deletion of knowledge.

---

## 7. CROSSREF — a doc asserting another file's state

**Test: read the other file.** Any sentence naming a file plus a claim about its
contents is a candidate:

```
"`X` still says …"      "see the section in `Y`"      "`Z` is currently …"
"as of writing, `A` …"  "`B` does not yet handle …"
```

**This is the pattern a trim creates.** Deleting a section makes every doc that
pointed at it wrong, and the deleting pass is the one least likely to look. One
audit produced three of these, all caused by its own trim an hour earlier:

```markdown
`CLAUDE.md`'s per-repo sections remain the narrative deep-dive doc.   # deleted
[`../../CLAUDE.md`] — per-repo notes for each model                   # deleted
(`run_eval.py:24-28` still says 8 …)                                  # fixed
```

**Prevention beats detection here.** Two rules:

1. **After any deletion pass, grep for references to what was deleted before
   reporting the pass complete.** Section headings, file names, distinctive
   phrases. This is Phase 7 of the workflow, not an optional extra.
2. **Never write a note describing another file's defect.** It becomes wrong the
   moment the defect is fixed, and nothing links the two. Either fix that file
   and write nothing, or record the durable requirement:

   | Don't | Do |
   |---|---|
   | "`run_eval.py:24` still says 8" | fix `run_eval.py`, write nothing |
   | "`config.py` doesn't validate this yet" | "Validation lives in `config.py`" |

A pointer to a file is fine. A pointer to a file's *current state* is a
time bomb.

---

## 8. SELF-REFERENTIAL — the doc narrating its own revision history

**Test:** does the sentence describe the *document's* past rather than the
*subject's* present? If removing the self-reference loses no fact and no
warning, cut it. If it carries a warning, **reframe it forward** rather than
deleting it.

The tells: "an earlier version of this file said", "this used to claim",
"corrected 2026-10-01", "why this was wrong for two days", "I did not check",
"this invalidated an earlier conclusion", "## Correction —".

This is distinct from `NARRATIVE`. Narrative is history *of the system*;
self-referential is history *of the writing*. It accumulates fast wherever a
doc is revised by an agent that has just corrected itself, and it has three
costs: it is dead weight to every future reader, it dates the document, and in
a work product that leaves the team it reads as churn rather than engineering.

**Cut** — autobiography with no payload:
```markdown
**An earlier version of this file claimed the opposite.** It does not survive.

(An earlier version of this line said "the only repo in the sweep"; the table
above lists `voxcpm1_5` at 44.1 kHz too, so that was wrong on its own page.)

## Correction — an earlier version of this guide was wrong

### Method note — why this was wrong for two days
```

**Reframe, do not delete** — the same passages carrying a real warning, turned
to face forward:
```markdown
`voxcpm1_5` is 44.1 kHz too; Audio8 is the only one of the pair that learns a
speaker rather than cloning from a prompt clip.

## Blockers — what `requirements.txt` implies vs what testing showed

### Method note — score the transcript, not the duration

"Fine-tuning destroys the instruct channel" is a confound, and an easy one to
reach: if the baseline is measured on CustomVoice while the fine-tune
initialises from `-Base`, the channel appears to vanish.
```

Note what survived each rewrite: the measurement, the trap, and the instruction
to the next reader. Only the first person and the timeline went.

### The distinction that matters: doc history vs code history

**A code comment explaining why an implementation is shaped the way it is is a
KEEPER**, even when it opens with "an earlier version". It stops someone
reverting to the broken approach, which is the keeper test exactly:

```python
# An earlier version called snapshot_download() with no patterns and pulled
# 818 GB for 12 German hours. Always pass hf_allow_patterns.      <- KEEP

# An earlier version counted quotes per line, which flagged every multi-line
# string. Count across the whole file instead.                     <- KEEP
```

Ask: **is it about the document's wording, or the code's behaviour?** Wording is
self-referential and goes; behaviour is a failure mode and stays.

Provenance in a compliance or audit record also stays — "licence corrected
2026-09-09, source re-fetched" is data lineage, not autobiography.

---

## 9. KEEP — non-recoverable knowledge

**Test:** could a competent engineer with full repo access derive this by
reading the code in about two minutes? If no, it stays.

Length is not a reason to cut. A three-sentence warning that prevents a
production outage earns its tokens.

**Always keep:**
- Failure modes with causes — "don't use `requests` here, it breaks under gevent"
- Version pins with reasons — "`numpy<2`, torch 2.1 segfaults on 2.x"
- Environment gotchas — "needs `libpq-dev` on Debian or the build fails silently"
- External constraints — licence terms, API quotas, rate limits, legal rules
- Ordering requirements — "run migrations before seeding or the FK fails"
- Landmines — "the retry logic looks redundant; it is not, see the duplicate
  webhook problem"
- Anything whose removal would let someone reintroduce a fixed bug

**The *because* test.** A "because" that names a real failure is a keeper. A
"because" that names a preference is narrative.

| Line | Flag |
|---|---|
| "We use pytest because it's nicer than unittest" | NARRATIVE |
| "Use pytest, not unittest — async fixtures deadlock under unittest" | KEEP |
| "The config is in YAML because the team likes YAML" | NARRATIVE |
| "Config must stay YAML — the deploy tool parses it directly" | KEEP |

**When uncertain, keep.** Then list it in the report under "kept despite looking
cuttable, reason: …". A wrongly kept line costs a few tokens; a wrongly deleted
one costs an outage and cannot be recovered from the code, which is precisely
why it was written down.

---

# False-positive discipline

Every finding is backed by a command result, never by reading and concluding.
**Report the candidates checked and discarded**, with the reason — that is what
makes the surviving findings credible, and it is the difference between an audit
and a list of guesses. One real sweep discarded four; each is a rule now.

### Resolve a path against every plausible root

A guide cited `TTS_infer_pack/TTS.py`; a `test -e` from the repo root said
missing. It exists at `GPT_SoVITS/TTS_infer_pack/TTS.py` — the doc states it
relative to the *package* root, which is how the code's own imports read it.

Before flagging `PHANTOM`, try: the containing directory, the repo root, the
owning package/clone root, and `git log --diff-filter=D` for a rename.

```bash
find . -name "$(basename "$p")" -not -path './.git/*' | head
```

### Establish what a number counts before calling it wrong

"17 are German-capable" looked stale: the manifest had 20 leaves at `german:
yes`, and 15 repos with at least one. Neither is 17 — so it was reported as
drift. It was **correct**: 17 is the count of repos with a `yes` *or* `partial`
leaf. The unit was unstated, not the number wrong.

A count is only stale once you have reproduced the figure it claims to be. If
no reading of the data produces it, say which readings you tried.

### Take the last match on a line, not the first

A date check flagged `verified 2026-09-04, re-verified 2026-09-08` as 4 days
stale. The line was current; the regex took the first of two dates. One false
positive in 25 rows is enough to teach a reader to skip the output.

### Word-boundary every pattern

A deferral sweep for `pending` matched **appending**, **depending** and
**spending** — 9 of 13 hits were noise. Anchor the pattern, then eyeball what
survives:

```bash
grep -rniE '\b(pending|deferred|not yet|TBD)\b' --include='*.md' .
```

### The meta-rule

If a check produces more false positives than findings, the check is wrong —
fix the check before reporting anything from it. A quote-balance heuristic
that counts quotes per line flags every comment containing an apostrophe --
eight false hits against one real one in practice. Replace a check like that;
do not keep it with caveats.
