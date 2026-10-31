---
name: md-to-pdf
description: Turn a Markdown file into a styled A4 PDF report (dark title, italic subtitle, clickable contents with real page numbers, lavender section headings, clean tables, real color emoji, embedded images). Use when the user gives a .md file and wants a PDF of it, asks to "make this a pdf", "export the report", or "render the md as pdf".
---

# Markdown → PDF report

The user gives a `.md` file; the output is a PDF next to it with the same name
(`Report 14-10-2026.md` → `Report 14-10-2026.pdf`). Plain white A4 document,
no cover: dark bold title, italic grey subtitle, Catppuccin Latte lavender
section headings with an underline, Inter throughout, real color emoji.

Renders by driving headless Chromium via Playwright — real CSS (`float`,
`:nth-child`, `text-transform`, `transparent`), and critically, real color
emoji, which no pure-Python PDF engine (WeasyPrint, xhtml2pdf/reportlab) can
draw at all: they can only read a font's plain vector outline table, never a
font's color/bitmap glyph data (COLR/CPAL, CBDT, sbix) — a structural
limitation of those renderers, not a font choice. Chromium can draw it.

## One-time setup per machine

```bash
uv run --no-project --with playwright playwright install chromium
```

Downloads Chromium (~100-300MB) into a per-user cache dir
(`~/.cache/ms-playwright` or the OS equivalent) — no sudo, no system package
manager, but it IS a real download, separate from `uv`'s own per-run package
cache. Only needs doing once per machine; cached after.

## Run it

```bash
uv run --no-project --with markdown --with playwright --with pymupdf --with fonttools \
    python ~/.claude/skills/md-to-pdf/md_to_pdf.py "<path/to/file.md>"
```

`pymupdf` resolves real page numbers for the clickable contents page (see
below) and checks font coverage for the unsupported-character warning.
`fonttools` reads the bundled fonts' cmaps for that same check.

The script's own `ensure_fonts()` downloads Inter (woff2, ~25KB/style) and
Noto Color Emoji (~25MB TTF, no smaller substitute exists with these glyphs
in color) into `fonts/` next to the script on first run — **not** committed
to the dotfiles repo (that'd bloat its git history forever for every clone);
`fonts/` is `.gitignore`d there. Same pattern as the Chromium download above:
one-time per machine, cached after. If `fonts/` is empty or missing, it just
re-downloads — nothing else needs touching.

Options: `-o out.pdf` (another name or folder), `--title "…"` (override the
title), `--no-toc`, `--no-subtitle`.

**Naming:** if the user wants the dated house format, name it
`Report DD-MM-YYYY.pdf` with `-o` (or rename the `.md` first) — ask only if the
date is not obvious from the file.

## How the Markdown is read

| Markdown | becomes |
|---|---|
| first `# Heading` | document title |
| first paragraph after it | italic grey subtitle (`--no-subtitle` to keep it as body text) |
| further `# Heading`s, if any | lavender sections with an underline; `##` subsections; `###` minor headings |
| otherwise | `##` are the lavender sections; `###` subsections |
| sections + subsections | clickable contents page with real page numbers |
| `![alt](path)` | full-width figure, path relative to the .md file |
| any emoji | real color glyph (Noto Color Emoji, via the font-family fallback stack) |

Tables get uppercase grey headers, a dark rule, light row lines and zebra
shading; rows avoid splitting across pages and headers repeat on continuation.

## After rendering — always check it

Use `pymupdf` via `uv` instead of `pdfinfo`/`pdftoppm` (poppler) — same reason
the renderer itself avoids a system-package dependency wherever avoidable.

1. Page count and a render of the first few pages — title page, a
   table-heavy page, a figure page:
   ```bash
   uv run --no-project --with pymupdf python -c "
   import fitz
   doc = fitz.open('<out.pdf>')
   print('pages:', doc.page_count)
   for i in range(min(3, doc.page_count)):
       doc[i].get_pixmap(dpi=100).save(f'<scratchpad>/prev-{i+1}.png')
   "
   ```
2. Read the PNGs. Fix anything that looks wrong before reporting done.
3. Check stderr for the unsupported-character warning (see below) — if it
   fired, tell the user which characters won't show up.
4. Tell the user the output path and page count.

## Known pitfalls (already handled — don't undo)

- **Fonts are downloaded, never committed, never system-resolved.**
  `ensure_fonts()` fetches Inter (`.woff2`, from Fontsource) and Noto Color
  Emoji (`.ttf`, from Google's `google/fonts` repo) into `fonts/` next to
  this script on first run — see `.gitignore`'s `md-to-pdf/fonts/` entry.
  Never switch this to a system font path (`/System/Library/Fonts`,
  `/usr/share/fonts`, `C:\Windows\Fonts`, ...) or commit the binaries to the
  repo — the former reintroduces a per-machine dependency this skill exists
  to avoid (a headless box can have zero fonts installed), the latter bloats
  the dotfiles repo's git history by 25MB+ forever.
- **`raw.githubusercontent.com`, not jsDelivr's GitHub proxy, for the emoji
  font.** jsDelivr refuses to proxy files over 20MB; Noto Color Emoji is
  ~25MB. jsDelivr's dedicated npm/Fontsource mirror is used for Inter instead
  (small `.woff2` files, no size issue there).
- **Why a one-time Chromium download is an acceptable trade-off here.**
  WeasyPrint needs Pango/Cairo/GObject (sudo or conda-forge); xhtml2pdf
  avoids native deps entirely but can't draw color emoji at all, at any
  weight. Playwright's Chromium download is the same shape of trade-off as
  the font downloads above — no sudo, cached per-machine after the first run
  — chosen because real color emoji mattered enough to accept it.
- **`table:not(.toc)` on the zebra-stripe rule.** A bare `<tr>` inside any
  `<table>` gets auto-wrapped in an implicit `<tbody>` by the browser, so
  `tbody tr:nth-child(even)` alone would *also* stripe the hand-built TOC
  table (which reuses plain `<tr>`s) — not just the content tables it's
  meant for.
- **The TOC is hand-built, not a browser/print-engine feature.** Chromium's
  print-to-PDF has no "give me each section's real page number" mechanism.
  `main()` renders twice: pass 1 (page numbers unknown yet, placeholders)
  locates each heading's real page via `locate_pages()` (pymupdf text
  search, skipping page 1 since the TOC page itself contains the same
  heading labels as link text and would false-match before the real heading
  does); pass 2 renders the final PDF with `build_toc_html()`'s table-based
  TOC where the whole row — both the label cell and the page-number cell —
  is a real `<a href="#id">` link, which Chromium's print-to-PDF *does*
  preserve as a clickable internal PDF link (confirmed via
  `page.get_links()` after rendering — link rects span nearly the full row
  width, not just a few points around the digit).
- **Unsupported-character warning.** `check_unsupported_chars()` loads both
  bundled fonts' cmaps via `fontTools` and scans the post-`restyle()` HTML
  (tags stripped) for codepoints neither font covers — mainly CJK, which
  Inter doesn't include. This is deliberately conservative: Chromium itself
  might render a CJK character fine via some system font that happens to be
  installed on *this* particular machine, but the check only counts what's
  guaranteed present everywhere (the bundled fonts), not what one machine
  happens to have — consistent with the whole point of bundling fonts in the
  first place. Scan runs on HTML *after* `restyle()`, not the raw markdown
  source, so it's checking exactly what reaches the renderer.
- **Emoji need no special handling in Python at all** — just the
  `font-family: "Inter", "Noto Color Emoji"` fallback stack in `_css()`.
  Chromium picks the right font per-character on its own, same as any real
  browser; there's no span-wrapping or codepoint substitution step (an
  earlier version had one, carried over from the reportlab-based renderer
  that genuinely needed it — removed once this stopped being reportlab).
- **Two render passes write to the same temp `.html` file** (via
  `tempfile.NamedTemporaryFile`, cleaned up in a `finally:` block) and
  `page.goto("file://...")` it, rather than `page.set_content()` — a page
  whose origin is `about:blank` (what `set_content()` gives you) can run
  into local-file-access restrictions loading `@font-face src: url("file://...")`
  that a real `file://`-origin page doesn't hit.
- `<base href="file://{markdown's own directory}/">` in the rendered HTML's
  `<head>` is what makes `![alt](relative/path.png)` resolve correctly
  regardless of where the temp HTML file itself lives.
- One shared spacing scale (`XS`/`SM`/`MD`/`LG`/`XL` = 1/2/3/4/6mm) and one
  shared border-weight scale (`BORDER_HAIRLINE`/`STRONG`/`ACCENT`/`BAR`) are
  used everywhere instead of ad-hoc per-element values, so vertical rhythm
  and line weights read as consistent rather than each block picking its
  own. Reuse these constants for any new element instead of hand-picking a
  new value.
- `blockquote p { margin: 0; }` — without it, the inner paragraph's own
  margin-bottom stacks with the blockquote's padding, leaving the left
  accent bar visibly taller than the text.
- python-markdown needs a blank line before a list that follows a paragraph;
  the script inserts it (outside code fences).
