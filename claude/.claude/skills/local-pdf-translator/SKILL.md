---
name: local-pdf-translator
description: Translate PDF documents (letters, contracts, forms) into another language entirely offline/locally, preserving the original layout, tables, logos, and formatting. Use when the user wants a PDF translated without sending it to a cloud service, especially for private/sensitive documents (bank letters, legal/government forms, contracts). Triggers on requests like "translate this PDF but keep it offline", "translate my documents without uploading them", "local PDF translator".
---

# Local PDF Translator

Translates PDFs layout-preserving, fully on the user's own hardware — the PDF
content is never sent to Claude or any cloud API, only extracted text snippets
go to Ollama/OCR. Translation and OCR compute are REQUIRED to run on the
user's remote NVIDIA GPU machine (`192.168.178.100`), not on the Mac this
script runs from — both `OLLAMA_URL` and `--ocr-url` default to that box's
addresses, and the script refuses to start (or silently fall back locally) if
either points at localhost/127.0.0.1/0.0.0.0, unless `--allow-local` is
explicitly passed. This includes the argos-translate offline fallback — it's
disabled by default too, since it runs on this machine, not the remote GPU
box. Don't add a "just fall back to local" path without `--allow-local` — that
was an explicit user requirement, not an oversight.

**Before running the script**, ask the user to confirm the GPU box's address
rather than silently relying on the `192.168.178.100` default — this skill may
be synced across multiple machines/networks where that IP isn't reachable.
Ask once per session (not per file): "Is your GPU box still at
`192.168.178.100`, or should I use a different `OLLAMA_URL`/`--ocr-url` this
time?" Skip asking only if the user already gave a host in this conversation,
or if `OLLAMA_URL`/`OCR_URL` are already exported in the shell.

## Output location convention

If the input PDF lives in a folder that already has `originals/` and
`translated/` subfolders (and ideally an `INDEX.md`) — e.g. the user's ING
documents folder — follow that convention: save the translated PDF into
`translated/<original-name>_EN.pdf`, and remind the user to add a row to
`INDEX.md` (original link, translated link, one-line description of the
document). Otherwise, default to saving `<name>_EN.pdf` next to the original,
as before.

## How it works

1. Extract text as positioned units via PyMuPDF (`fitz`):
   - Normal horizontal blocks keep PyMuPDF's multi-line grouping (bbox
     correctly bounds genuinely related stacked lines, e.g. a 4-line address).
   - Rotated/vertical text (e.g. sideways reference-code labels along a
     margin) and any block with mixed reading directions are broken apart
     and handled **per line**, each with its own line-level bbox and
     rotation — PyMuPDF sometimes groups multiple unrelated vertical labels
     that are far apart on the page into one "block" whose bbox spans the
     entire gap between them, so merging their text would force lines that
     were never adjacent into one string.
2. Deduplicate identical text (repeated footers/disclaimers across many
   pages) and translate each *unique* string in its own request, JSON-
   constrained (`{"translation": "..."}`), in parallel via a thread pool.
   Never batch multiple blocks into one shared numbered-list prompt — that
   was the original design and it silently dropped/misaligned blocks
   whenever one block had embedded newlines (e.g. a multi-line address).
3. White-out each original text region and redraw the translation in the
   same spot, preserving bold/italic, alignment, and rotation:
   - For rotated text, redraw with `insert_textbox(..., rotate=...)` using
     the line's own tight bbox — don't expand it (it's a narrow column by
     design).
   - For normal text, first try growing the box *downward* into whatever
     free space exists before the next block below it (`compute_safe_bottom`)
     rather than immediately shrinking the font — this is what keeps font
     size visually consistent with surrounding text instead of one block
     looking abnormally small just because its translation needed one more
     line than the original.
   - Only shrink font size as a last resort, and do it correctly: the loop
     must actually attempt the floor font size, not stop one step before it
     (an off-by-one here silently dropped whole blocks that would have fit).
4. Run a validation pass: re-open the saved output and compare per-page word
   counts against the source, flagging any page where translated content
   looks like it dropped by more than half. Also report any block whose
   translation request failed after retries, or that never fit even at the
   minimum font size.

## Known bugs already fixed here (don't reintroduce)

- **Batched numbered-line translation** (`"1|||text\n2|||text..."` in one
  prompt) breaks the moment any block has an embedded newline — the model's
  own multi-line output desyncs the regex parser and blocks after it get
  dropped or merged. Always translate blocks individually.
- **Blindly expanding the redraw box** (e.g. `x1 + 40, y1 + 20`) to give
  translated text room to wrap lets it bleed into whatever block sits nearby
  when the translation needs an extra line. Pad by ~1-2pt and let the font-
  shrink loop do the work instead.
- **Rotated/vertical text** (check each line's `dir` tuple — not `(1, 0)`
  means rotated) gets a garbled bbox from `get_text("dict")` (extremely
  narrow and tall) and explodes into one-character-per-line if you naively
  join all its lines and redraw with `insert_textbox` at the *block* bbox.
  `insert_textbox` does support a `rotate=0/90/180/270` param and works fine
  — the fix is to (a) actually pass `rotate`, and (b) extract rotated text
  per-LINE with each line's own bbox, not per-block, since PyMuPDF can group
  multiple unrelated vertical labels that are pages-apart-vertically into
  one block whose bbox spans the entire gap between them.
- **Off-by-one in the font-shrink loop**: `while rc < 0 and fontsize > 4:
  fontsize -= 0.5` can step past the exact size that would have fit (e.g.
  it fits at exactly 4.0 but the loop condition `fontsize > 4` stops one
  iteration before trying it) — this silently dropped an entire block that
  had no room to spare. Use a loop that guarantees the floor size itself
  gets attempted, and log a warning if even the floor doesn't fit rather
  than silently leaving a blank box.
- **Expanding a redraw box only vertically, into free space below** (via
  `compute_safe_bottom`, checking for the next block with an overlapping
  x-range) keeps font size consistent with the rest of the page. Don't
  expand horizontally — that risks bleeding into a side-by-side column
  (e.g. a two-column address/date layout).
- **The model may still insert/remove line breaks** it wasn't asked to,
  splitting an originally single-line label into two cramped lines. The
  prompt now explicitly demands the exact same line count in and out, but
  it isn't 100% reliable — treat this as a cosmetic residual risk, not
  something to chase further.
- **`insert_textbox`'s default `lineheight` reserves more vertical room per
  line than a single line actually needs** — this alone can force an
  unnecessary shrink on a tight single-line label even though the text
  visually fits fine at the original font size (confirmed: a 9pt single
  line needing ~97pt of a 99pt-long box was still reported as not fitting,
  by a near-constant ~6pt deficit that didn't budge no matter how much
  extra length was given — the deficit was in *thickness*, not length).
  Passing `lineheight=0.7` for single-line-only inserts (skip it — leave
  the default — for genuinely multi-line text) removes that padding and
  gets the original font size back. Extending the box along its length axis
  first (see `compute_safe_extend`, the rotated-text analogue of
  `compute_safe_bottom`) still matters for genuinely multi-line content.
  **Only apply the tight override when the block is BOTH rotated AND was a
  single line in the SOURCE** (check the original text, not the
  translation) — a horizontal block that was one line in the source can
  still wrap into several lines after translation, and tightening
  lineheight there reproduces the overlap bug below.
- **`insert_textbox` commits pixels immediately on every call — it is NOT a
  dry-run/measurement call**, even when its return value says the text
  didn't fit. The font-shrink retry loop must re-white the box before EVERY
  attempt, not just once before the loop, or a failed attempt's partial
  text stays on the page and the next (smaller-font) attempt draws on top
  of it — this produced visibly doubled, overlapping paragraph text that
  looked like a rendering glitch but was actually two separate real
  insertions stacked on each other.
- **`compute_safe_bottom` must also treat images as obstacles**, not just
  other text blocks — otherwise a text block sitting just above a signature
  image can expand downward into it, whiting out part of the signature.
  Pull image boxes once per page via `page.get_image_info()`.
- **Base-14 fonts ("helv" etc.) render € (and some other non-ASCII chars) as
  a literal "?"** — confirmed on a real fee schedule full of Euro amounts.
  Find a real TTF (`find_font_file`, checks common per-OS paths — Arial on
  macOS, Liberation/DejaVu Sans on Linux, arial.ttf on Windows) and pass it
  via `insert_textbox(fontfile=...)`; give each bold/italic combination its
  own `fontname` alias when using a fontfile, or PyMuPDF can conflate
  different embedded files under one shared alias. Only when no such file
  exists at all should you fall back to Base-14 + sanitizing known-bad
  characters (`sanitize_for_base14`: € → "EUR", etc.) — that's a degraded
  path, not the default.
- **Whiteout must sample the block's actual background color, not assume
  white.** A section-header banner with white text on a dark (maroon)
  background went completely invisible: the hardcoded white whiteout erased
  the dark background right under where the (correctly white) translated
  text was redrawn, so white-on-white disappeared. Snapshot the page with
  `page.get_pixmap()` ONCE before touching anything (mutating the live page
  and re-sampling from it would pick up already-edited content), then for
  each block sample the mode color over a coarse grid within its bbox
  (`sample_bg_color` — glyphs are sparse strokes, so the mode reliably lands
  on background, not glyph color) and use THAT as both the whiteout fill and
  compare against for readable text color.
- **PyMuPDF can group side-by-side table COLUMNS on one row into a single
  "block" with multiple "lines"** — confirmed on a real country/currency
  table: a block came out as `"Albanien\nAL\nAlbanischer Lek\nALL"` (4
  columns of one row) with a bbox only 8pt tall (one row's height). Treating
  it as 4 stacked paragraph lines demands 4 lines' worth of height in an
  8pt box — impossible at any font size, hence "did not fit even at 3pt"
  and the whole row silently failing. Detect this (`is_same_row_columns`:
  all lines' vertical centers within `0.5 × median line height` of each
  other despite there being multiple lines) and handle each column as its
  own independent single-line block, the same way rotated text is already
  handled per-line rather than per-block.
- **Cap `compute_safe_bottom`'s downward expansion** (e.g. `y1 + max(20, 4 ×
  row_height)`), don't leave it unbounded. In a dense two-column price
  table, the "next block in this x-range" can be a whole section away, and
  an uncapped search returns nearly the full remaining page — letting one
  row's translated text visibly drift down into an unrelated row several
  lines below. This was the biggest single quality gap on any table-heavy
  page; the other fixes above (banner colors, € encoding) are cosmetic by
  comparison.

## Prerequisites — check before running

```bash
python3 -c "import fitz" || pip3 install pymupdf
python3 -c "import requests" || pip3 install requests
```

Ollama must be reachable (local or LAN) with a model pulled:
```bash
ollama list
```
If nothing suitable is there, pull a solid instruction model, e.g.:
```bash
ollama pull qwen2.5:14b
```
(~9GB, runs comfortably on 16GB+ VRAM or a modern CPU; use `qwen2.5:7b` for
lighter hardware, `qwen2.5:32b` for 24GB+ VRAM and higher quality on dense
legal/contract text.)

### Using a remote/LAN GPU machine instead of local Ollama

If the user has a more powerful machine (e.g. NVIDIA GPU box) on their network:

1. On that machine, Ollama must bind to all interfaces, not just localhost:
   ```bash
   OLLAMA_HOST=0.0.0.0 ollama serve
   ```
   (or via systemd: `sudo systemctl edit ollama`, add
   `Environment="OLLAMA_HOST=0.0.0.0"`, then `daemon-reload` + `restart`.)
   Also ensure the firewall allows port 11434 from the LAN.
2. Verify reachability from this machine:
   ```bash
   curl -s -m 5 http://<remote-ip>:11434/api/tags
   ```
3. Point the script at it via env vars (see below).

## Running

```bash
export OLLAMA_URL="http://<host>:11434/api/generate"   # default localhost:11434
export OLLAMA_MODEL="qwen2.5:14b"                        # must match a pulled model

python3 ~/.claude/skills/local-pdf-translator/scripts/translate_pdf.py \
  <input.pdf> <output.pdf> --source German --target English --workers 4
```

`--workers` controls how many blocks are translated concurrently (unique
blocks only, thanks to caching) — 4 is a reasonable default; raise it if the
GPU box has headroom, since each request is small.

- Check page count first (`fitz.open(path)` → `len(doc)`) to set expectations —
  a 5-page letter finishes in seconds; an 80+ page contract can take a long
  time on CPU. Prefer a GPU (local or LAN) for anything beyond ~20 pages, and
  offer to run it in the background with progress tracking (the script prints
  `PROGRESS page N/M: ...` to stderr per page).
- For multiple documents, translate sequentially against the same Ollama
  instance rather than in parallel, to avoid GPU/VRAM contention.
- After completion, render a couple of sample pages to PNG and read them back
  to visually confirm layout held up before telling the user it's done:
  ```python
  import fitz
  doc = fitz.open(output_pdf)
  doc[0].get_pixmap(dpi=110).save("/tmp/check.png")
  ```

## OCR for scanned pages

If a page has no extractable text (`get_text("dict")` returns nothing) but
does contain images, it's likely scanned. Pass `--ocr-url` (or set `OCR_URL`)
pointing at a PaddleOCR GPU server on the LAN (see the paddleocr-gpu-server
setup — `server.py` + `start_server.bat`, exposes `POST /ocr` accepting a
multipart `file` and returning `{"results": [{"text", "confidence", "box"}]}`
with `box` as 4 pixel-coordinate corners):

```bash
export OCR_URL="http://<gpu-box-ip>:8000"
python3 ~/.claude/skills/local-pdf-translator/scripts/translate_pdf.py \
  <input.pdf> <output.pdf> --ocr-url "$OCR_URL"
```

`ocr_page()` renders the page to a PNG at 200dpi, posts it to `<ocr_url>/ocr`,
and converts each result's pixel-space box into a PDF-point bbox (scale
factor `72/dpi`) with an estimated font size (`~0.75 × box height`) — this
produces the exact same `(text, info)` shape as `extract_blocks()`, so OCR'd
pages flow through the identical translate/cache/redraw/validate pipeline as
normal text pages, no separate code path needed downstream. Two extra steps
specific to OCR, both learned from an actual bad first pass:

- **Filter out logo/signature garbage before translating anything.** OCR
  can't distinguish a company logo (e.g. "ING" rendered inside a graphic) or
  a scrawled signature from real text — it just reads pixels. Both look the
  same to a simple heuristic: short text (≤8 chars) in an abnormally large
  box relative to the page's real body text (`> 2.2 × median line height`).
  Without this, a first test run whited out the ING lion logo and both
  signatures and replaced them with a giant literal rendering of the
  misread gibberish ("marcuf", "fh ya").
- **Group adjacent OCR lines into paragraph blocks before translating**
  (`group_ocr_lines`) — PaddleOCR returns one result per detected line, and
  translating each line in isolation loses sentence context, producing
  visibly disjointed phrasing across a wrapped paragraph. Group lines that
  are vertically adjacent (gap `< 0.9 × median line height`) and share a
  left margin (`within 2 × median line height`), the same idea as how
  PyMuPDF groups real text into blocks.

Check reachability before relying on it: `curl -m 5 <ocr_url>/health` — if it
times out, the server likely isn't running (`start_server.bat` on the GPU
box) or is blocked by its Windows Firewall rule ("PaddleOCR API", TCP 8000).
Without `--ocr-url` set, scanned pages are just flagged with a warning and
left untranslated, same as before OCR support existed.

## Known limitations

- Very dense tables/forms where the translated text is longer than the
  German/source text can get slightly truncated in narrow cells — the script
  shrinks font size to compensate but there's a floor (3pt). Flag this to the
  user rather than silently shipping cut-off text if a check reveals it.
- OCR-derived text has no reliable bold/italic/alignment/rotation info (the
  PaddleOCR API here doesn't report font metrics) — OCR'd blocks are always
  redrawn plain, left-aligned, non-rotated. Fine for scanned bank/legal
  letters; would need enhancement for scanned documents with rotated text.
- The logo/signature filter is a heuristic (short text + oversized box), not
  a classifier — a genuinely short, large-font heading on a scanned page
  (rare, but possible) could get skipped too. Reasonable trade-off for the
  bank/legal-letter case this was built for; revisit if it misfires on a
  different kind of document.
