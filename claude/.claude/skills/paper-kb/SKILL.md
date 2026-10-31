---
name: paper-kb
description: "Personal research knowledge base over a real Zotero library and Obsidian vault. Ingest a paper (Zotero item + exhaustive colour-coded highlights + a dense linked note), query what you know, cluster concepts into research forks, or branch out into new directions grounded in your own notes. Domain-agnostic. Use when the user says \"add this paper\", \"ingest paper\", \"what do I know about X\", \"branch out on X\", or mentions their paper knowledge base."
argument-hint: "[ingest \"title\" doi:<doi> | query \"topic\" | branch-out \"topic\" | cluster | stats | lint]"
allowed-tools: Bash(*), Read, Write, Edit, Grep, Glob, WebSearch, WebFetch, Agent, mcp__zotero__*, mcp__obsidian-vault__*
---

# Paper Knowledge Base

Subcommand + args: **$ARGUMENTS**

A vault of paper notes is only worth more than the papers themselves if it
records what you *concluded*, not what each paper *said*. Everything below
exists to force that: adjudicate claims against the paper's own tables,
verify artifacts against the release rather than the text, and name the
relationships between papers explicitly. **Density over brevity throughout** —
when unsure whether something earns a highlight, a note line, or a concept
note, include it.

## Prerequisites

```bash
uv tool install zotero-mcp-server
claude mcp add zotero -s user -- zotero-mcp -e ZOTERO_LOCAL=true
claude mcp add obsidian-vault -s user -- npx @bitbonsai/mcpvault@latest /path/to/vault
```
`claude mcp list` must show both. If either is missing, **stop and say so** —
this skill writes into both systems and has no degraded mode.

## Vault layout

```
papers/<slug>.md            one note per paper — the primary unit
concepts/<slug>.md          atomic cross-cutting mechanisms only
ideas/idea-NNN-<slug>.md    research directions from branch-out
_meta/research_brief.md     the real goal — read FIRST for query/branch-out
_meta/concept_clusters.md   concept taxonomy by problem solved — highest signal
_meta/gap_map.md            open gaps, stable IDs (G1…), never renumbered
_meta/index.md              one verdict line per paper
_meta/edges.jsonl           typed relationship graph (sidecar, not .md)
_meta/log.md                append-only audit trail
_meta/stats.md              counts snapshot (derived)
```

`node_id`: `paper:<slug>`, `concept:<slug>`, `idea:<id>`. Slug =
`<first-author-surname><year>_<keyword>`, **always equal to the filename** so
`[[wikilinks]]` resolve.

**Two tool constraints, both discovered the hard way.** The `obsidian-vault`
write tool accepts only `.md` paths — use plain `Write`/`Edit` for
`edges.jsonl` (the vault is a normal directory). And **never open Zotero's
live `zotero.sqlite`**: all content goes through the `zotero` MCP. For
library-wide aggregates only (annotation counts, colour distributions), copy
**all three** files — `.sqlite`, `-wal`, `-shm` — to a temp dir, run
`PRAGMA wal_checkpoint(TRUNCATE)`, query the copy, delete it. Copying the
`.sqlite` alone silently misses everything still in the write-ahead log.

**PDFs are resolved on demand** via `zotero_get_attachment_path(item_key=…)`
using the note's `zotero_key`. Never mirror Zotero's `storage/` tree.

## Paper note

Frontmatter: `type` · `node_id` · `title` · `authors` · `year` · `venue` ·
`doi` · `zotero_key` · `tags` · `relevance` (`core`/`related`/`peripheral`) ·
`verification_status` · `added`. Then, in order:

`One-line thesis` · `Problem / Gap` · `Method` · `Key Results` · `Claims` ·
`Assumptions` · `Limitations / Failure Modes` · `Reusable Ingredients` ·
`Connections` · `Open Questions` · `Interesting / My Insights` ·
`Abstract (original)`

Papers that are not method papers — datasets, benchmarks, corpora, repo-only
releases — may legitimately add or rename sections (e.g. `Licensing —
critical finding`). Method papers should not.

### `## Claims` is the section that makes the vault worth having

**Not a list of links. A numbered adjudication of the paper's own assertions
against the paper's own tables**, each marked ✅ supported / ⚠ qualified /
⛔ contradicted, with section numbers. Quote the claim verbatim where the
wording is doing work.

The value concentrates here because the recurring finding in any literature
is that **abstracts overstate what the tables show**. Check every time:

- **Does a headline claim survive its own table?** "Best on every axis" that
  loses one cell; "consistent performance" contradicted four rows later.
- **Are all promised metrics reported?** A methods paragraph listing four
  measures against a results table showing two is a finding, not an
  oversight — and the missing one is usually where the cost landed.
- **Does anything regress, and where is it disclosed?** A win in the abstract
  with the matching loss paraphrased in an appendix is the standard shape.
- **Is the cleanest protocol the one showing the worst result?** It often is.
- **Is a headline number a best-of-N selection knob?** Then the deployable
  number is the single-pass one.
- **Is the "hard" split actually harder**, or just drawn in-domain?

### Baseline and ceiling — demand both, every time

**A score is uninterpretable without the do-nothing number and the real-data
number beside it.** Record both when given, and record their *absence* when
not:

- **Floor** — what does the null intervention score? Copy the input, skip the
  method, answer at chance. Methods routinely sit within noise of it.
- **Ceiling** — what does the *real* artifact score under the same
  instrument? Papers often compute this, print it once, and never carry it
  into the headline, where it would lower the claim rather than raise it.

This is not pedantry: applied carefully it has turned a published positive
result into a significant result *in the opposite direction*. If a paper
reports neither, say so in `Claims`. If it reports one and never reads it,
that is the finding.

### Artifacts and licensing — verify against the release, never the paper

Papers are routinely silent, stale, or wrong about what they shipped. Check
the actual GitHub/HF/Zenodo release and record the date. **Three independent
axes — never collapse them:**

| Axis | Question |
|---|---|
| `license_code` | What governs the repo? |
| `license_weights` | What governs the released artifacts? Often different. |
| `license_data` | What governs the training corpus? **Where encumbrance usually hides.** |

A permissive tag on a derivative of a restrictive parent does not launder it;
a derivative cannot be more permissive than its parent. Watch for: a repo
`LICENSE` contradicting its own README; a licence sitting beside a "research
use only" line; per-checkpoint tags differing inside one org; undeclared
training data under a permissive weight tag; and **reimplementations of
restrictively-licensed designs** — for those, check the dependency manifest
and where the artifacts ship from, since architecture *ideas* carry no
licence obligation but copied code or weights do.

Report "no licence file" and "a licence the API cannot auto-detect" as
different findings — some auto-detectors return nothing for valid licences.

*Fields with no released artifacts can skip this — say so explicitly rather
than leaving it blank.*

### Audit the instrument, not just the result

Before trusting a number, ask whether the measuring apparatus is sound:

- **Circularity** — does any model or extractor appear on **both** sides of
  the training/evaluation boundary? Conditioning on a predictor then scoring
  with it measures invertibility, not quality. Within-model ablation is fine;
  cross-system comparison is not. The extractor can also be a *label source*,
  which hides the trail across sections.
- **Degenerate baseline** — what does a do-nothing system score? If it's a
  good score, the metric needs a partner.
- **Bespoke metrics** — a newly invented measure with no human-correlation
  study is a hypothesis. Count how many the field now has.
- **Underpowered or non-blind evaluation** — rater counts, screening,
  blinding, significance tests. **Compute the test the paper omits** when the
  data allows it.
- **Agreement vs κ** — raw agreement and correlation flatter a judge on
  skewed binary data in a way κ does not. If only agreement is reported, that
  is a reporting choice.

## Concept note

Frontmatter: `type` · `node_id` · `name` · `tags` · `added`.
Sections: `Definition` · `Introduced / used by` (one bullet per paper,
wikilinked, **with that paper's specific instantiation and numbers**) ·
optional `Tradeoffs` · `My notes`.

Only for genuinely cross-cutting, nameable, reusable mechanisms — never a
mechanical per-section split of one paper. But err toward **more** concepts:
a rich method section usually yields several.

**Match before creating.** Grep `concepts/` first. If a note exists, append a
bullet rather than making a near-duplicate — and where the new instance
differs in a way that matters, write the comparison into the existing note.
Two concepts that look alike but solve *different* problems stay separate,
with the distinction stated in both.

## Edges

`_meta/edges.jsonl`, one object per line:
`{"from": "paper:x", "to": "paper:y", "type": "...", "evidence": "..."}`

**Exactly six valid types. Do not invent others:**
`extends` · `contradicts` · `supports` · `tested_by` · `supersedes` ·
`inspired_by`

`evidence` must cite specific sections and numbers **from both ends** —
one-line justifications are not the house style.

⚠ **If no type fits, say so and write prose instead of forcing one.** The
commonest unrepresentable relation is the **sibling pole**: two papers
solving one problem differently, with no lineage, no shared benchmark and no
test of each other. `supports` overstates it; `extends` is false. Record it
in both notes' `Connections` and in the relevant cluster.

**Never assert an edge without the user confirming it.** Proposing edges and
logging "edges: proposed, awaiting confirmation" is a normal end state.

## `ingest "<title>" doi:<doi>`

For a long PDF, **fork a subagent per paper** so raw text doesn't flood the
main context.

1. **Resolve and verify.** DOI → CrossRef; arXiv ID → arXiv API. Never
   fabricate an identifier. If the API rate-limits, build the item from the
   PDF itself and say so. Set `verification_status`.
2. **Create the Zotero item**, attach the PDF.
3. **Decide the note's `tags` first** — they are the source of truth (see Tag
   sync).
4. **Read the PDF in full.** Not the abstract, not the first pages.
5. **Highlight exhaustively** via `zotero_create_annotation`, section by
   section: every component, loss term, training stage, hyperparameter,
   headline table number, tradeoff sentence and limitation. A handful of
   highlights on a multi-section paper is a failure, not a shortcut. Typical
   density: **60–150 per paper.** Every highlight takes a substantive
   `comment` — the colour says what kind of thing it is, the comment says
   what it means.

   **Five colours, fixed vault-wide. Never invent a per-paper scheme:**

   | Colour | Hex | Means |
   |---|---|---|
   | 🔴 Red | `#ff6666` | **Contradiction or correction** — the paper is wrong, overstates, contradicts itself or the vault |
   | 🟡 Yellow | `#ffd400` | **Reframes an existing vault finding.** Use sparingly |
   | 🔵 Blue | `#2ea8e5` | **Licence, release, availability, provenance** |
   | 🟢 Green | `#5fb236` | **Method and results.** The bulk of an ingest |
   | 🟣 Purple | `#a28ae5` | **Cross-reference or scope caveat** (n=2 subjects, one backbone, eval-only) |

   Two pinned edge cases: a **licence conflict is blue**, not red — a
   provenance fact, not a claim about the work. **Underpowered or non-blind
   evaluation is purple**, not red — it qualifies scope. Reserve red for a
   stated result being wrong, unsupported, or contradicted by its own tables.

6. **Write `papers/<slug>.md`** — all sections, `Claims` **populated**.
7. **Mine concepts** (match before creating).
8. **Propose edges**, do not write them.
9. **Set `relevance`** against `research_brief.md`'s stated priority order,
   and **state the reason in the note**, not just the field. A paper can be
   `core` on mechanism while being unusable in practice — say which.
10. **Append one line to `_meta/log.md`.** Dense and pipe-delimited: what was
    found, what was *corrected*, what surprised you. The log is the only
    audit trail; a thin line wastes the ingest.
11. **Update `index.md`**; refresh `research_brief.md` if a `core` paper or a
    gap's evidence picture changed.

### Verify every relayed figure against the source

When a number reaches you from a summary, a sweep, another paper or an
earlier note rather than from the PDF in front of you, **check it**. In
practice this surfaces roughly **one material error per three papers** —
transposed values, a figure attributed to the wrong table, a range quoted as
a point estimate, a competitor's number read as the paper's own.

**Finding one is a success.** It belongs in a red highlight, in `Claims` and
in the log line. Correct the source note too, so the error stops propagating.

### Papers that do not exist

Some of the most relevant artifacts — strong repo releases, corpora, model
cards — have no paper. **Ingest them anyway**; a paper-gated pipeline will
systematically miss well-engineered releases.

Deviations, all of which must be **stated in a header at the top of the
note** so they read as source limitations rather than skipped steps: Zotero
item type `webpage`/`computerProgram`; `doi: none` with the reason inline;
**no highlight pass**; **no `Claims` adjudication** — there are no
claims-against-tables to adjudicate, though README assertions can be recorded
as unverified claims. Everything else — architecture read from config files
and source, licence verification, concept mining, edges, log line — proceeds
normally, and is often *more* reliable than a paper's self-report.

### Tag sync (Obsidian → Zotero, one-directional, exact match)

The note's `tags:` frontmatter is the source of truth. Make the Zotero item's
tags **exactly match** — add what's missing, remove everything else,
including auto-populated subject classifications (arXiv categories and
similar). Curated tags replace those; they don't coexist. Re-run whenever a
note's tags change, not only at ingest.

## `query "<topic>"`

Regenerate `_meta/query_pack.md` to a fixed budget, then answer **by citing
specific notes — never from memory**.

| Section | Budget | Content |
|---|---|---|
| Research direction | full | Verbatim Problem/Constraints/Direction/Non-Goals |
| Top gaps | 1200 ch | Unresolved, ranked by linked ideas + divergent clusters |
| Clusters | 1600 ch | Divergent first, 2–3 sentences each |
| Failed ideas | 1400 ch | **Always included** — highest anti-repetition value |
| Top papers | 1800 ch | 8–12, `core` first |
| Open questions | 500 ch | Unresolved across the vault |

Over budget: cut `peripheral` papers first, then cluster detail. **Never**
prune failed ideas or top gaps — those are what prevent re-proposing dead
ends.

## `branch-out "<topic>"`

1. Search the vault **and** the web/arXiv/OpenAlex. For an arXiv-concentrated
   literature the arXiv API alone is often the better instrument — it returns
   verified identifiers by construction and cannot hallucinate one.
2. Read `query_pack.md` + `gap_map.md`.
3. **Verify each new candidate** against arXiv/CrossRef, then run the full
   `ingest` on the **top 5**; list the rest as plain citations. Branch-out
   must grow the knowledge base, not just generate ideas and discard what it
   found.
4. Favour directions serving the brief's **Direction** over ones that merely
   fill a gap.
5. Check novelty against existing notes — don't re-propose a tried-and-failed
   idea.
6. Write survivors to `ideas/idea-NNN-<slug>.md`; log each.
7. **Triage honestly.** If a cluster of candidates cannot answer the question
   you actually have — e.g. they all report a metric family structurally
   blind to the effect in question — **close them without ingesting, record
   why, and write a concrete reopening condition.** Seven thin ingests are
   worse than one decisive experiment. Never drop candidates silently.

## `cluster`

Rebuild `_meta/concept_clusters.md`, grouping concepts by **what problem they
solve**, not by paper or tag. Name each cluster with the question it poses.

Classify each: **convergent** (independent papers, same mechanism — evidence
it is load-bearing) · **divergent/forked** (same problem, different
mechanisms — a live research question) · **singleton**.

**Build the axis from the evidence, not from a taxonomy you brought with
you.** A good cluster has an axis that *predicts*, not one that merely sorts.
If the ingests suggest a better axis than the one you planned, take theirs
and say what changed. Multi-axis clusters are normal once a fork matures.

**Requirements:** every concept gets exactly one primary placement; a concept
shown elsewhere for context is marked `×` and not counted there. Publish the
placement arithmetic and make it sum. Verify zero unplaced concepts and zero
dangling wikilinks before finishing.

**Deferral is legitimate.** If a fork's poles are mostly un-ingested,
building it fixes the axis with the odd pole missing. Record the deferral and
its unblocking condition instead — but don't let one blocked cluster hold up
concepts that aren't blocked.

⭐ **A cluster turning divergent is the strongest "research this now" signal
this skill has.** Surface it proactively in the report.

## `stats`

Regenerate `_meta/stats.md`: papers by relevance, unverified count, concepts
(placed/held), edges by type, ideas, gaps, clusters, divergent count.

⚠ **Derived views drift and hand-written health claims lie.** Count from disk
every time. If a prose "lint clean" line disagrees with a script, the script
wins — and fix the prose. Mark anything authored-not-derived in its own
header so a later regeneration doesn't overwrite it.

## `lint`

Reports only, writes nothing. Check: orphan concepts (referenced by no paper
note) · unplaced concepts · dangling wikilinks · missing frontmatter keys
(`relevance`, `zotero_key`, `verification_status`, `doi`) · `node_id` not
matching filename · **missing `## Claims` body** · notes sharing 2+ tags with
no edge · concept notes with a single "Introduced / used by" entry · stale
ideas (14+ days) · `unverified` papers (14+ days) · tag drift against
Zotero · non-kebab-case tags.

Two recurring false positives worth knowing: a *tag* written as a
`[[wikilink]]` reads as dangling, and a log line quoting a link it describes
fixing will re-flag forever — escape it.

## Key rules

- **Never fabricate** a DOI, arXiv ID, metric or metadata. If a lookup fails,
  say so and ask.
- **Never assert an edge** without confirmation. Six types only; prose for
  the rest.
- **Verify artifacts against the release**, never the paper. Three licence
  axes, separately.
- **Populate `Claims`** with adjudication against the paper's own tables. An
  empty `Claims` header is the defect this skill exists to prevent.
- **Demand a floor and a ceiling** for every headline score.
- **Verify relayed figures.** ~1 material error per 3 papers; finding one is
  a success.
- **Density over brevity**: exhaustive highlighting, a note capturing every
  number worth remembering, every distinct mechanism promoted to a concept.
- Every ingest sets `relevance` + `verification_status` and writes a log line.
- Gap IDs are **stable once assigned** — never renumber.
- Never write to Zotero's data directory; never open the live sqlite.
- **State what you did not do.** Scope cut, cluster deferred, candidate
  closed, question unresolved — flag it rather than letting silence imply
  completeness.
- If `zotero` or `obsidian-vault` is unavailable, **stop** — there is no
  degraded mode.
