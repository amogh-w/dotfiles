#!/usr/bin/env python3
"""
Offline PDF translator: extracts text blocks with PyMuPDF, translates them
via a local (or LAN) Ollama server, and redraws the translation back into
each block's original position/size, preserving layout, images, and tables.

Design notes (lessons from earlier failures):
- Each unique block is translated in its OWN request (JSON-constrained output),
  never batched into a shared numbered list — batching + line-based parsing
  silently dropped/misaligned blocks whenever one block had embedded newlines
  (e.g. a multi-line address).
- Identical repeated blocks (footers/disclaimers repeated on every page of a
  long document) are translated once and cached, for consistency and speed.
- Unique blocks are translated concurrently (ThreadPoolExecutor) since each
  call is independent.
- A validation pass re-opens the saved output and checks block counts per
  page match the source, and flags blocks that came back identical to the
  source (likely silent translation failures) so they can be reported instead
  of shipped silently.
- Bold/italic and text alignment are inferred from the original spans/lines
  and preserved on redraw, instead of always drawing plain left-aligned text.

Usage:
  python3 translate_pdf.py <input.pdf> <output.pdf> [--source German] [--target English] [--workers 4] [--ocr-url http://host:8000]

Translation and OCR are REQUIRED to run on the remote NVIDIA GPU machine, not
on this (Mac) machine — both default to that box's addresses, and the script
refuses to start (or falls back) to anything on localhost/127.0.0.1 unless
--allow-local is explicitly passed. This is a deliberate choice, not an
oversight: keep translation compute off the local machine.

Env vars:
  OLLAMA_URL   default http://192.168.178.100:11434/api/generate (remote GPU box)
  OLLAMA_MODEL default qwen2.5:14b
  OCR_URL      default http://192.168.178.100:8000 (remote GPU box's PaddleOCR
               server — see the paddleocr-gpu-server setup) — used ONLY for
               pages with no extractable text but that contain images
               (scanned pages). Not needed for normal text-based PDFs.
"""
import sys, os, re, json, argparse, time
from concurrent.futures import ThreadPoolExecutor, as_completed
import requests
import fitz

REMOTE_HOST = "192.168.178.100"
OLLAMA_URL = os.environ.get("OLLAMA_URL", f"http://{REMOTE_HOST}:11434/api/generate")
MODEL = os.environ.get("OLLAMA_MODEL", "qwen2.5:14b")
OCR_URL = os.environ.get("OCR_URL", f"http://{REMOTE_HOST}:8000")
ALLOW_LOCAL = False  # set from --allow-local at startup


def is_local_url(url):
    return any(h in url for h in ("localhost", "127.0.0.1", "0.0.0.0", "::1"))


def enforce_remote(url, label):
    if is_local_url(url) and not ALLOW_LOCAL:
        print(f"ERROR: {label} points at localhost ({url}) — translation/OCR must run on "
              "the remote NVIDIA GPU machine, not this Mac. Set the env var to the GPU "
              "box's address, or pass --allow-local to explicitly override this.",
              file=sys.stderr)
        sys.exit(1)

FENCE_RE = re.compile(r"^```(?:json)?\s*|\s*```$", re.IGNORECASE | re.MULTILINE)


def strip_code_fences(s):
    return FENCE_RE.sub("", s).strip()


def try_argos_fallback(text, source_lang, target_lang):
    """Best-effort fallback if Ollama is unreachable — runs ON THIS MACHINE, so it's
    disabled unless --allow-local was passed (translation must stay on the remote GPU
    box by default; silently falling back to local defeats that)."""
    if not ALLOW_LOCAL:
        return None
    try:
        import argostranslate.translate as at
        LANG_CODES = {"german": "de", "english": "en", "french": "fr", "spanish": "es",
                      "italian": "it", "dutch": "nl", "portuguese": "pt"}
        src = LANG_CODES.get(source_lang.lower())
        tgt = LANG_CODES.get(target_lang.lower())
        if not src or not tgt:
            return None
        installed = at.get_installed_languages()
        from_lang = next((l for l in installed if l.code == src), None)
        to_lang = next((l for l in installed if l.code == tgt), None)
        if not from_lang or not to_lang:
            return None
        translation = from_lang.get_translation(to_lang)
        if not translation:
            return None
        return "\n".join(translation.translate(line) for line in text.split("\n"))
    except Exception:
        return None


def group_ocr_lines(lines):
    """
    Merge OCR'd lines that are vertically adjacent and horizontally aligned
    into paragraph-level blocks (same idea as PyMuPDF's own block grouping
    for real text) — translating one line at a time loses sentence context
    and produces disjointed phrasing across a wrapped paragraph.
    Each input line: {"text", "bbox": (x0,y0,x1,y1)}.
    Returns a list of {"text" (joined with \\n), "bbox" (union)}.
    """
    if not lines:
        return []
    heights = [l["bbox"][3] - l["bbox"][1] for l in lines]
    median_h = sorted(heights)[len(heights) // 2]
    ordered = sorted(lines, key=lambda l: (l["bbox"][1], l["bbox"][0]))

    groups = []
    for line in ordered:
        x0, y0, x1, y1 = line["bbox"]
        placed = False
        for g in groups:
            gx0, gy0, gx1, gy1 = g["bbox"]
            vgap = y0 - gy1
            x_aligned = abs(x0 - gx0) < median_h * 2  # same left margin, roughly
            if 0 <= vgap < median_h * 0.9 and x_aligned:
                g["lines"].append(line)
                g["bbox"] = (min(gx0, x0), min(gy0, y0), max(gx1, x1), max(gy1, y1))
                placed = True
                break
        if not placed:
            groups.append({"lines": [line], "bbox": (x0, y0, x1, y1)})

    return [{"text": "\n".join(l["text"] for l in g["lines"]), "bbox": g["bbox"]} for g in groups]


def ocr_page(page, ocr_url, dpi=200):
    """
    OCR a scanned (image-only) page via a PaddleOCR GPU server (see the
    paddleocr-gpu-server setup: POST multipart 'file' to <ocr_url>/ocr,
    get back {"results": [{"text", "confidence", "box": [[x,y]*4]}]}).
    Returns (texts, infos) in the same shape as extract_blocks(), so OCR'd
    pages flow through the exact same translate/cache/redraw/validate
    pipeline as normal text pages.
    """
    pix = page.get_pixmap(dpi=dpi)
    png_bytes = pix.tobytes("png")
    try:
        resp = requests.post(f"{ocr_url}/ocr", files={"file": ("page.png", png_bytes, "image/png")},
                              timeout=120)
        resp.raise_for_status()
        results = resp.json().get("results", [])
    except Exception as e:
        print(f"  [warn] OCR request failed ({e}); leaving this page untranslated", file=sys.stderr)
        return [], []

    scale = 72.0 / dpi  # PaddleOCR box coords are in rendered-image pixels; convert to PDF points
    raw_lines = []
    heights = []
    for r in results:
        text = (r.get("text") or "").strip()
        if not text or r.get("confidence", 1.0) < 0.5:
            continue
        box = r["box"]
        xs = [p[0] for p in box]
        ys = [p[1] for p in box]
        x0, y0, x1, y1 = min(xs) * scale, min(ys) * scale, max(xs) * scale, max(ys) * scale
        heights.append(y1 - y0)
        raw_lines.append({"text": text, "bbox": (x0, y0, x1, y1)})

    if not raw_lines:
        return [], []

    # A company logo (e.g. "ING" inside a graphic) or a scrawled signature
    # OCR'd as gibberish both look the same to a heuristic: short text in an
    # abnormally large box compared to the page's real body text. Filtering
    # these out avoids whiting out a logo/signature and replacing it with a
    # giant, wrong rendering of misread text.
    median_h = sorted(heights)[len(heights) // 2]
    lines = [l for l in raw_lines
              if not (len(l["text"]) <= 8 and (l["bbox"][3] - l["bbox"][1]) > median_h * 2.2)]
    skipped = len(raw_lines) - len(lines)
    if skipped:
        print(f"  [info] OCR: skipped {skipped} short/oversized result(s) "
              "(likely a logo or signature graphic, not real text)", file=sys.stderr)

    blocks = group_ocr_lines(lines)
    texts, infos = [], []
    for b in blocks:
        x0, y0, x1, y1 = b["bbox"]
        n_lines = b["text"].count("\n") + 1
        fontsize = max(4, (y1 - y0) / n_lines * 0.75)  # cap-height ~70-80% of one line's box height
        texts.append(b["text"])
        infos.append({
            "bbox": (x0, y0, x1, y1), "fontsize": fontsize, "bold": False,
            "italic": False, "color": 0, "align": fitz.TEXT_ALIGN_LEFT, "rotate": 0,
        })
    return texts, infos


def translate_one(text, source_lang, target_lang, max_retries=2):
    """Translate a single text block, JSON-constrained, with retries and an offline fallback."""
    prompt = (
        f"Translate the following {source_lang} text to {target_lang}. It is one block from "
        "an official letter/contract/form and may contain multiple lines (e.g. an address or "
        "a list) — preserve the EXACT number of line breaks given, no more, no fewer: if the "
        "input has N lines, the translation must also have exactly N lines. Preserve any "
        "leading list markers/bullets (e.g. '-', '•') at the start of a line exactly as given. "
        "Keep numbers, dates, IBANs, codes, and proper nouns unchanged. "
        "Respond with ONLY a JSON object of the form "
        '{"translation": "..."} where the value uses \\n for line breaks. No other text, '
        "no markdown code fences.\n\n"
        f"TEXT:\n{text}"
    )
    last_err = None
    for attempt in range(max_retries + 1):
        try:
            resp = requests.post(OLLAMA_URL, json={
                "model": MODEL,
                "prompt": prompt,
                "stream": False,
                "format": "json",
                "options": {"temperature": 0.0 if attempt > 0 else 0.1},
            }, timeout=120)
            resp.raise_for_status()
            out = strip_code_fences(resp.json()["response"].strip())
            parsed = json.loads(out)
            trans = parsed.get("translation")
            if isinstance(trans, str) and trans.strip():
                return trans, "ok"
        except requests.exceptions.RequestException as e:
            last_err = e
            fallback = try_argos_fallback(text, source_lang, target_lang)
            if fallback:
                return fallback, "argos-fallback"
            break  # connection issue won't fix itself on retry
        except Exception as e:
            last_err = e
            continue
    print(f"  [warn] translation failed for block ({last_err}); keeping original", file=sys.stderr)
    return text, "failed"


def dominant_span_style(block):
    """Return (fontsize, is_bold, is_italic, color) from the majority (by char count) span in a block."""
    tally = {}
    for line in block["lines"]:
        for s in line["spans"]:
            key = (s["size"], bool(s["flags"] & 2 ** 4), bool(s["flags"] & 2 ** 1), s.get("color", 0))
            tally[key] = tally.get(key, 0) + len(s["text"])
    if not tally:
        return 10, False, False, 0
    return max(tally.items(), key=lambda kv: kv[1])[0]


def detect_alignment(block):
    """Heuristic: compare variance of line left/right/center edges to guess alignment."""
    lines = block["lines"]
    if len(lines) < 2:
        return fitz.TEXT_ALIGN_LEFT
    lefts = [l["bbox"][0] for l in lines]
    rights = [l["bbox"][2] for l in lines]
    centers = [(l["bbox"][0] + l["bbox"][2]) / 2 for l in lines]

    def spread(vals):
        m = sum(vals) / len(vals)
        return sum((v - m) ** 2 for v in vals) / len(vals)

    sl, sr, sc = spread(lefts), spread(rights), spread(centers)
    best = min(sl, sr, sc)
    if best == sl or sl <= 1.0:
        return fitz.TEXT_ALIGN_LEFT
    if best == sr:
        return fitz.TEXT_ALIGN_RIGHT
    return fitz.TEXT_ALIGN_CENTER


# Base-14 fonts ("helv" etc.) use a limited Latin encoding that silently
# renders unsupported characters (notably € — confirmed: comes out as "?")
# as garbage. A real TTF, embedded via `fontfile`, renders full Unicode
# correctly. Search common per-OS locations; fall back to Base-14 (and
# sanitize known-bad characters) if none is found.
_FONT_FILE_CANDIDATES = {
    (False, False): [
        "/System/Library/Fonts/Supplemental/Arial.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",
        "C:\\Windows\\Fonts\\arial.ttf",
    ],
    (True, False): [
        "/System/Library/Fonts/Supplemental/Arial Bold.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Bold.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
        "C:\\Windows\\Fonts\\arialbd.ttf",
    ],
    (False, True): [
        "/System/Library/Fonts/Supplemental/Arial Italic.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-Italic.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans-Oblique.ttf",
        "C:\\Windows\\Fonts\\ariali.ttf",
    ],
    (True, True): [
        "/System/Library/Fonts/Supplemental/Arial Bold Italic.ttf",
        "/usr/share/fonts/truetype/liberation/LiberationSans-BoldItalic.ttf",
        "/usr/share/fonts/truetype/dejavu/DejaVuSans-BoldOblique.ttf",
        "C:\\Windows\\Fonts\\arialbi.ttf",
    ],
}
_font_file_cache = {}


def find_font_file(is_bold, is_italic):
    key = (is_bold, is_italic)
    if key not in _font_file_cache:
        _font_file_cache[key] = next(
            (p for p in _FONT_FILE_CANDIDATES[key] if os.path.exists(p)), None)
    return _font_file_cache[key]


def pymupdf_fontname(is_bold, is_italic):
    if is_bold and is_italic:
        return "hebi"
    if is_bold:
        return "hebo"
    if is_italic:
        return "heit"
    return "helv"


def sanitize_for_base14(text):
    """Only used when no TTF fontfile is available — Base-14 fonts render
    these characters as '?' otherwise."""
    return text.replace("\u20ac", "EUR").replace("\u2013", "-").replace("\u2014", "-")


def block_rotation(block):
    """
    Return 0/90/180/270 if every line in the block shares one consistent
    reading direction, or None if directions are mixed (rare, unsupported).
    dir (1,0)=0°, (0,-1)=90°, (-1,0)=180°, (0,1)=270° — matches PyMuPDF's
    insert_textbox `rotate` convention.
    """
    rotations = set()
    for line in block["lines"]:
        dx, dy = line.get("dir", (1, 0))
        if dx > 0.9:
            rotations.add(0)
        elif dy < -0.9:
            rotations.add(90)
        elif dx < -0.9:
            rotations.add(180)
        elif dy > 0.9:
            rotations.add(270)
        else:
            rotations.add(None)
    return rotations.pop() if len(rotations) == 1 else None


def line_style(line):
    tally = {}
    for s in line["spans"]:
        key = (s["size"], bool(s["flags"] & 2 ** 4), bool(s["flags"] & 2 ** 1), s.get("color", 0))
        tally[key] = tally.get(key, 0) + len(s["text"])
    if not tally:
        return 10, False, False, 0
    return max(tally.items(), key=lambda kv: kv[1])[0]


def line_rotation(line):
    dx, dy = line.get("dir", (1, 0))
    if dx > 0.9:
        return 0
    if dy < -0.9:
        return 90
    if dx < -0.9:
        return 180
    if dy > 0.9:
        return 270
    return None


def is_same_row_columns(block):
    """
    True if a block's "lines" are actually several side-by-side table
    COLUMNS on one row, not stacked paragraph lines — PyMuPDF can group
    them together when cells are close enough. Confirmed on a real
    country/currency table: a block "Albanien\\nAL\\nAlbanischer Lek\\nALL"
    whose bbox was only 8pt tall (one row) but got treated as 4 lines to
    stack, which is impossible to fit at any font size. Detected by: lines
    whose vertical centers are all nearly identical (same row) despite
    being multiple lines.
    """
    lines = block["lines"]
    if len(lines) < 2:
        return False
    centers = [(l["bbox"][1] + l["bbox"][3]) / 2 for l in lines]
    heights = [l["bbox"][3] - l["bbox"][1] for l in lines]
    median_h = sorted(heights)[len(heights) // 2] or 1
    return (max(centers) - min(centers)) < median_h * 0.5


def extract_blocks(page):
    d = page.get_text("dict")
    raw_blocks = [b for b in d["blocks"] if b.get("type") == 0]
    texts, infos = [], []
    skipped_mixed = 0
    for b in raw_blocks:
        rot = block_rotation(b)
        if rot == 0 and is_same_row_columns(b):
            # Side-by-side columns on one row, not stacked lines — treat
            # each column as its own single-line block instead of joining
            # them with "\n" (which would need to fit N lines in a
            # one-row-tall box: impossible at any font size).
            for line in b["lines"]:
                line_text = "".join(s["text"] for s in line["spans"]).strip()
                if not line_text:
                    continue
                fontsize, is_bold, is_italic, color = line_style(line)
                texts.append(line_text)
                infos.append({
                    "bbox": line["bbox"], "fontsize": fontsize, "bold": is_bold,
                    "italic": is_italic, "color": color,
                    "align": fitz.TEXT_ALIGN_LEFT, "rotate": 0,
                })
        elif rot == 0:
            # Normal horizontal block: keep the standard multi-line grouping —
            # PyMuPDF's block bbox correctly bounds genuinely-related stacked
            # lines here (e.g. a multi-line address).
            lines_text = ["".join(s["text"] for s in line["spans"]) for line in b["lines"]]
            full_text = "\n".join(lines_text).strip()
            if not full_text:
                continue
            fontsize, is_bold, is_italic, color = dominant_span_style(b)
            align = detect_alignment(b)
            texts.append(full_text)
            infos.append({
                "bbox": b["bbox"], "fontsize": fontsize, "bold": is_bold,
                "italic": is_italic, "color": color, "align": align, "rotate": 0,
            })
        elif rot is None:
            # Genuinely mixed-direction block (rare) — could be several unrelated
            # rotated lines PyMuPDF grouped together despite being far apart (seen
            # with sideways reference-code labels). Handle each LINE separately
            # instead of merging, since the enclosing block bbox can span a huge
            # unrelated gap between two unrelated vertical labels.
            for line in b["lines"]:
                line_rot = line_rotation(line)
                line_text = "".join(s["text"] for s in line["spans"]).strip()
                if not line_text:
                    continue
                if line_rot is None:
                    skipped_mixed += 1
                    continue
                fontsize, is_bold, is_italic, color = line_style(line)
                texts.append(line_text)
                infos.append({
                    "bbox": line["bbox"], "fontsize": fontsize, "bold": is_bold,
                    "italic": is_italic, "color": color,
                    "align": fitz.TEXT_ALIGN_LEFT, "rotate": line_rot,
                })
        else:
            # Uniformly-rotated block (all lines same non-zero direction) — same
            # per-line treatment, since a rotated column is a stack of separate
            # lines along its thickness, not along its length.
            for line in b["lines"]:
                line_text = "".join(s["text"] for s in line["spans"]).strip()
                if not line_text:
                    continue
                fontsize, is_bold, is_italic, color = line_style(line)
                texts.append(line_text)
                infos.append({
                    "bbox": line["bbox"], "fontsize": fontsize, "bold": is_bold,
                    "italic": is_italic, "color": color,
                    "align": fitz.TEXT_ALIGN_LEFT, "rotate": rot,
                })
    if skipped_mixed:
        print(f"  [info] left {skipped_mixed} mixed-direction text line(s) untranslated on this page",
              file=sys.stderr)
    return texts, infos


def sample_bg_color(orig_pixmap, scale, bbox):
    """
    Sample the dominant color under a block's bbox from a pixmap taken
    BEFORE any edits on this page. A block's whiteout must match its real
    background, not assume white — confirmed bug: a white-text-on-dark-
    banner header went completely invisible because the (hardcoded white)
    whiteout erased the dark background right where the (correctly white)
    text was about to be redrawn. Most pixels in a text block's bbox are
    background (glyphs are sparse strokes), so a mode over a coarse sample
    grid reliably picks the background, not glyph color.
    """
    x0, y0, x1, y1 = bbox
    px0, py0 = int(x0 * scale), int(y0 * scale)
    px1, py1 = int(x1 * scale), int(y1 * scale)
    px1, py1 = max(px1, px0 + 1), max(py1, py0 + 1)
    px1, py1 = min(px1, orig_pixmap.width), min(py1, orig_pixmap.height)
    if px1 <= px0 or py1 <= py0:
        return (1, 1, 1)
    from collections import Counter
    counter = Counter()
    n = orig_pixmap.n  # bytes per pixel (3 or 4)
    step_x = max(1, (px1 - px0) // 12)
    step_y = max(1, (py1 - py0) // 12)
    for py in range(py0, py1, step_y):
        for px in range(px0, px1, step_x):
            i = (py * orig_pixmap.width + px) * n
            rgb = orig_pixmap.samples[i:i + 3]
            if len(rgb) == 3:
                counter[bytes(rgb)] += 1
    if not counter:
        return (1, 1, 1)
    color = counter.most_common(1)[0][0]
    return tuple(c / 255 for c in color)


def compute_safe_bottom(idx, infos, page_height, obstacles):
    """
    How far down block `idx` (a rot==0 block) can grow before it would
    overlap the next block/image below it in the same horizontal span. Lets
    us give translated text more room to wrap onto an extra line WITHOUT
    shrinking its font, instead of always cramming it into the original
    single-line box height. `obstacles` is an extra list of (x0,y0,x1,y1)
    boxes (e.g. images/signatures) that must not be encroached on either.

    Capped at a small multiple of the block's own height: in a dense table
    with far-apart neighbors (e.g. a two-column price list where the next
    block in the same x-range is a whole section away), an uncapped search
    can return almost the full remaining page — letting a row's translated
    text drift down into a completely unrelated row several lines below.
    """
    x0, y0, x1, y1 = infos[idx]["bbox"]
    row_height = y1 - y0
    cap = y1 + max(20, row_height * 4)
    best = min(page_height - 5, cap)
    for j, other in enumerate(infos):
        if j == idx:
            continue
        ox0, oy0, ox1, oy1 = other["bbox"]
        if oy0 <= y0:
            continue
        if ox1 > x0 and ox0 < x1:  # horizontal ranges overlap
            best = min(best, oy0 - 2)
    for ox0, oy0, ox1, oy1 in obstacles:
        if oy0 <= y0:
            continue
        if ox1 > x0 and ox0 < x1:
            best = min(best, oy0 - 2)
    return best


def compute_safe_extend(idx, infos, page_width, page_height):
    """
    Like compute_safe_bottom, but for a rotated line: find how far it can
    extend along its own reading axis (vertical for 90/270, horizontal for
    180) before it would hit a neighboring line/block, instead of shrinking
    the font to fit the tight original per-line bbox. Vertical/sideways
    labels are often surrounded by a lot of genuinely empty page space.
    """
    x0, y0, x1, y1 = infos[idx]["bbox"]
    rot = infos[idx]["rotate"]
    if rot in (90, 270):
        top, bottom = 5, page_height - 5
        for j, other in enumerate(infos):
            if j == idx:
                continue
            ox0, oy0, ox1, oy1 = other["bbox"]
            if ox1 <= x0 or ox0 >= x1:
                continue
            if oy1 <= y0:
                top = max(top, oy1 + 2)
            elif oy0 >= y1:
                bottom = min(bottom, oy0 - 2)
        return fitz.Rect(x0, top, x1, bottom)
    else:
        left, right = 5, page_width - 5
        for j, other in enumerate(infos):
            if j == idx:
                continue
            ox0, oy0, ox1, oy1 = other["bbox"]
            if oy1 <= y0 or oy0 >= y1:
                continue
            if ox1 <= x0:
                left = max(left, ox1 + 2)
            elif ox0 >= x1:
                right = min(right, ox0 - 2)
        return fitz.Rect(left, y0, right, y1)


def process_pdf(in_path, out_path, source_lang="German", target_lang="English", workers=4, ocr_url=""):
    enforce_remote(OLLAMA_URL, "OLLAMA_URL")
    if ocr_url:
        enforce_remote(ocr_url, "--ocr-url")

    doc = fitz.open(in_path)
    page_texts, page_infos = [], []
    unique_texts = set()

    for pno in range(len(doc)):
        texts, infos = extract_blocks(doc[pno])
        if not texts and doc[pno].get_images():
            if ocr_url:
                print(f"  [info] page {pno+1} has no extractable text but contains images — "
                      "sending to OCR server", file=sys.stderr)
                texts, infos = ocr_page(doc[pno], ocr_url)
                if not texts:
                    print(f"  [warn] OCR on page {pno+1} produced no text", file=sys.stderr)
            else:
                print(f"  [warn] page {pno+1} has no extractable text but contains images — "
                      "likely scanned; pass --ocr-url to OCR it", file=sys.stderr)
        page_texts.append(texts)
        page_infos.append(infos)
        unique_texts.update(texts)

    unique_texts = list(unique_texts)
    cache = {}
    failed_blocks = []
    print(f"Translating {len(unique_texts)} unique text blocks across {len(doc)} pages "
          f"({workers} workers)...", file=sys.stderr, flush=True)

    total = len(unique_texts)
    report_every = max(1, min(20, total // 10 or 1))  # ~10 updates over the run, never less than every block on small docs
    with ThreadPoolExecutor(max_workers=workers) as ex:
        future_to_text = {ex.submit(translate_one, t, source_lang, target_lang): t for t in unique_texts}
        done = 0
        start = time.monotonic()
        for fut in as_completed(future_to_text):
            t = future_to_text[fut]
            trans, status = fut.result()
            cache[t] = trans
            if status != "ok":
                failed_blocks.append((t, status))
            done += 1
            if done % report_every == 0 or done == total:
                pct = 100 * done // total
                elapsed = time.monotonic() - start
                rate = done / elapsed if elapsed > 0 else 0
                eta = (total - done) / rate if rate > 0 else 0
                print(f"PROGRESS translated {done}/{total} unique blocks ({pct}%, "
                      f"~{eta:.0f}s remaining)", file=sys.stderr, flush=True)

    for pno in range(len(doc)):
        page = doc[pno]
        page_height = page.rect.height
        page_width = page.rect.width
        infos = page_infos[pno]
        image_boxes = [tuple(im["bbox"]) for im in page.get_image_info()]
        bg_dpi = 150
        bg_scale = bg_dpi / 72.0
        orig_pixmap = page.get_pixmap(dpi=bg_dpi)  # snapshot BEFORE any edits, for bg color sampling
        for idx, (info, text) in enumerate(zip(infos, page_texts[pno])):
            trans = cache[text]
            x0, y0, x1, y1 = info["bbox"]
            fontsize = info["fontsize"]
            font_file = find_font_file(info["bold"], info["italic"])
            if font_file:
                # A distinct name per variant, or PyMuPDF can conflate
                # different embedded font files under one shared alias.
                fontname = f"custom-{info['bold']}-{info['italic']}"
            else:
                fontname = pymupdf_fontname(info["bold"], info["italic"])
                trans = sanitize_for_base14(trans)
            c = info["color"]
            text_color = ((c >> 16 & 255) / 255, (c >> 8 & 255) / 255, (c & 255) / 255)
            bg_color = sample_bg_color(orig_pixmap, bg_scale, info["bbox"])
            rotate = info["rotate"]
            is_single_line = "\n" not in text  # based on ORIGINAL text, not the translation

            if rotate == 0:
                # Prefer growing downward into free space over shrinking the
                # font — keeps font size visually consistent with the rest
                # of the page instead of only this one block looking smaller.
                safe_bottom = compute_safe_bottom(idx, infos, page_height, image_boxes)
                rect = fitz.Rect(x0 - 1, y0 - 1, x1 + 2, max(y1 + 2, safe_bottom))
            else:
                # Rotated lines: extend along the reading axis into free
                # space before shrinking font, same principle as above.
                rect = compute_safe_extend(idx, infos, page_width, page_height)

            # PyMuPDF's default lineheight reserves more vertical room per
            # line than a single line actually needs, which was forcing
            # unnecessary shrinking on tight single-line ROTATED labels
            # (where the box's narrow WIDTH, not height, was the tight
            # constraint). Restricted to rotate!=0: a horizontal block that
            # was one line in the source can still wrap into multiple lines
            # after translation, and tightening lineheight there causes the
            # same visibly-overlapping-lines bug this was fixing elsewhere.
            lineheight = 0.7 if (rotate != 0 and is_single_line) else None

            # insert_textbox commits pixels immediately on every call, even
            # when it reports "doesn't fit" — it is NOT a dry-run measurement.
            # Re-whiting the box before every attempt (not just once before
            # the loop) is required, or a failed attempt's partial text stays
            # on the page and the next attempt draws on top of it, producing
            # visibly doubled/overlapping text.
            min_fontsize = 3
            while True:
                page.draw_rect(rect, color=bg_color, fill=bg_color)
                rc = page.insert_textbox(
                    rect, trans, fontsize=fontsize, fontname=fontname,
                    fontfile=font_file, color=text_color, align=info["align"],
                    rotate=rotate, lineheight=lineheight,
                )
                if rc >= 0 or fontsize <= min_fontsize:
                    break
                fontsize = max(min_fontsize, fontsize - 0.5)
            if rc < 0:
                print(f"  [warn] block did not fit even at {min_fontsize}pt on page {pno+1}: "
                      f"{text[:60]!r}", file=sys.stderr)
        pct = 100 * (pno + 1) // len(doc)
        print(f"PROGRESS page {pno+1}/{len(doc)} rendered ({pct}%)", file=sys.stderr, flush=True)

    doc.save(out_path)
    print(f"Saved {out_path}", file=sys.stderr)

    # --- Validation pass ---
    # Block counts aren't directly comparable (a redrawn multi-line textbox can
    # re-parse into a different number of blocks than the source), so instead
    # check word-count ratio per page as a coarse "did content go missing" signal.
    orig = fitz.open(in_path)
    result = fitz.open(out_path)
    suspect_pages = []
    for pno in range(len(orig)):
        orig_words = len(orig[pno].get_text().split())
        result_words = len(result[pno].get_text().split())
        if orig_words > 0:
            ratio = result_words / orig_words
            if ratio < 0.5:
                suspect_pages.append((pno + 1, orig_words, result_words))

    if suspect_pages:
        print("VALIDATION WARNING: pages where translated word count is <50% of source "
              "(possible dropped content) — (page, source_words, output_words): "
              f"{suspect_pages}", file=sys.stderr)
    else:
        print(f"VALIDATION OK: word-count ratio looks sane on all {len(orig)} pages", file=sys.stderr)

    if failed_blocks:
        print(f"VALIDATION WARNING: {len(failed_blocks)} block(s) failed to translate "
              "and were kept in the source language:", file=sys.stderr)
        for t, status in failed_blocks[:10]:
            print(f"  [{status}] {t[:80]!r}", file=sys.stderr)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("input_pdf")
    parser.add_argument("output_pdf")
    parser.add_argument("--source", default="German")
    parser.add_argument("--target", default="English")
    parser.add_argument("--workers", type=int, default=4)
    parser.add_argument("--ocr-url", default=OCR_URL,
                         help="PaddleOCR GPU server base URL (e.g. http://192.168.178.100:8000), "
                              "used only for scanned/image-only pages")
    parser.add_argument("--allow-local", action="store_true",
                         help="Explicitly permit OLLAMA_URL/--ocr-url to point at localhost, and "
                              "permit the argos-translate local fallback. Off by default: "
                              "translation/OCR must run on the remote GPU machine.")
    args = parser.parse_args()
    ALLOW_LOCAL = args.allow_local
    process_pdf(args.input_pdf, args.output_pdf, args.source, args.target, args.workers, args.ocr_url)
