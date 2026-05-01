#!/usr/bin/env python3
"""Build per-tool learning guides under ``guides/`` into static HTML.

Source format
-------------
Each ``guides/<name>.md`` is a tiny Markdown subset:

  - ``# H1`` / ``## H2`` / ``### H3`` headings
  - ``- bullet`` lists (single level)
  - inline ``\\`code\\```, ``**bold**``, and ``[text](url)`` links
  - blank lines separate paragraphs
  - ```` ```quiz ```` fenced blocks whose body is TOML; each block is a
    ``[[questions]]`` array with ``q`` / ``options`` / ``answer`` / ``why``

Anything outside that subset is rendered as a literal paragraph.

Output
------
``guides/build/<name>.html`` plus copies of ``style.css`` and ``quiz.js``,
and an auto-generated ``index.html`` listing every guide.

Zero third-party dependencies — pure stdlib, including ``tomllib``
(Python 3.11+) for the quiz blocks.
"""

from __future__ import annotations

import argparse
import html
import re
import shutil
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent
SRC_DIR = ROOT
OUT_DIR = ROOT / "build"
TEMPLATE_PATH = ROOT / "template.html"
STATIC_ASSETS = ("style.css", "quiz.js")

QUIZ_FENCE = re.compile(r"^```quiz\s*\n(.*?)\n```\s*$", re.MULTILINE | re.DOTALL)
INLINE_LINK = re.compile(r"\[([^\]]+)\]\(([^)]+)\)")
INLINE_BOLD = re.compile(r"\*\*([^*]+)\*\*")
INLINE_ITALIC = re.compile(r"(?<!\*)\*([^*\s][^*]*?)\*(?!\*)")
INLINE_CODE = re.compile(r"`([^`]+)`")
HEADING = re.compile(r"^(#{1,3})\s+(.*)$")
BULLET = re.compile(r"^-\s+(.*)$")


def render_inline(text: str) -> str:
    """Apply inline transforms to an already-plain-text string.

    Order matters: links first (so backticks inside link text survive as
    code spans inside ``<a>``), then bold, then code.
    """
    text = html.escape(text)
    text = INLINE_LINK.sub(
        lambda m: f'<a href="{m.group(2)}">{m.group(1)}</a>',
        text,
    )
    text = INLINE_BOLD.sub(r"<strong>\1</strong>", text)
    text = INLINE_ITALIC.sub(r"<em>\1</em>", text)
    text = INLINE_CODE.sub(r"<code>\1</code>", text)
    return text


def render_block(text: str) -> str:
    """Render a non-quiz Markdown segment to HTML."""
    lines = text.splitlines()
    out: list[str] = []
    i = 0
    while i < len(lines):
        line = lines[i]
        if not line.strip():
            i += 1
            continue
        heading_match = HEADING.match(line)
        if heading_match:
            level = len(heading_match.group(1))
            inner = render_inline(heading_match.group(2))
            out.append(f"<h{level}>{inner}</h{level}>")
            i += 1
            continue
        if BULLET.match(line):
            bullets: list[str] = []
            while i < len(lines):
                bullet_match = BULLET.match(lines[i])
                if not bullet_match:
                    break
                bullets.append(f"  <li>{render_inline(bullet_match.group(1))}</li>")
                i += 1
            out.append("<ul>")
            out.extend(bullets)
            out.append("</ul>")
            continue
        # Paragraph: collect until blank line or block-level marker.
        para: list[str] = []
        while i < len(lines):
            cur = lines[i]
            if not cur.strip() or HEADING.match(cur) or BULLET.match(cur):
                break
            para.append(cur)
            i += 1
        out.append(f"<p>{render_inline(' '.join(para))}</p>")
    return "\n".join(out)


def render_quiz(toml_body: str, qid: str) -> str:
    """Render one ``\\`\\`\\`quiz`` block (TOML body) to a ``<form>``."""
    try:
        data = tomllib.loads(toml_body)
    except tomllib.TOMLDecodeError as exc:
        raise ValueError(f"quiz '{qid}': invalid TOML: {exc}") from exc
    questions = data.get("questions") or []
    if not isinstance(questions, list) or not questions:
        raise ValueError(f"quiz '{qid}' has no [[questions]]")

    out: list[str] = [f'<form class="quiz" data-qid="{qid}">']
    for i, q in enumerate(questions):
        if not isinstance(q, dict):
            raise ValueError(f"quiz '{qid}' question {i}: not a table")
        q_text = render_inline(str(q["q"]))
        options = q["options"]
        answer = int(q["answer"])
        why = render_inline(str(q.get("why", "")))
        why_attr = html.escape(why, quote=True)
        if not isinstance(options, list) or len(options) < 2:
            raise ValueError(f"quiz '{qid}' question {i}: needs ≥ 2 options")
        if not 0 <= answer < len(options):
            raise ValueError(f"quiz '{qid}' question {i}: answer {answer} out of range")
        name = f"{qid}-q{i}"
        out.append(
            f'  <fieldset class="question" data-answer="{answer}" data-why="{why_attr}">'
        )
        out.append(f"    <legend>{q_text}</legend>")
        for j, opt in enumerate(options):
            opt_html = render_inline(str(opt))
            out.append(
                f'    <label><input type="radio" name="{name}" value="{j}"> '
                f"{opt_html}</label>"
            )
        out.append('    <p class="feedback"></p>')
        out.append("  </fieldset>")
    out.append('  <div class="quiz-actions">')
    out.append('    <button type="button" class="check">Check answers</button>')
    out.append('    <button type="button" class="reset">Reset</button>')
    out.append(f'    <span class="score">0 / {len(questions)}</span>')
    out.append("  </div>")
    out.append("</form>")
    return "\n".join(out)


def render_doc(md_text: str, slug: str) -> tuple[str, str]:
    """Render a full Markdown doc, returning ``(title, body_html)``.

    The first ``# H1`` line becomes the page title; it is also rendered into
    the body so the page reads as a single document.
    """
    title = slug
    for line in md_text.splitlines():
        if line.startswith("# "):
            title = line[2:].strip()
            break

    parts: list[str] = []
    cursor = 0
    for quiz_n, m in enumerate(QUIZ_FENCE.finditer(md_text)):
        before = md_text[cursor : m.start()].strip("\n")
        if before:
            parts.append(render_block(before))
        parts.append(render_quiz(m.group(1), f"{slug}-quiz{quiz_n}"))
        cursor = m.end()
    tail = md_text[cursor:].strip("\n")
    if tail:
        parts.append(render_block(tail))

    return title, "\n\n".join(parts)


def render_index(entries: list[tuple[str, str]]) -> tuple[str, str]:
    """Render the auto-generated ``index.html`` listing all guides."""
    items = "\n".join(
        f'  <li><a href="{slug}.html">{html.escape(title)}</a></li>'
        for slug, title in sorted(entries)
    )
    body = f"<h1>Guides</h1>\n<ul>\n{items}\n</ul>"
    return "Guides", body


def load_template() -> str:
    return TEMPLATE_PATH.read_text(encoding="utf-8")


def wrap(template: str, *, title: str, slug: str, body: str) -> str:
    """Substitute placeholders in the wrapper template."""
    return (
        template.replace("__TITLE__", html.escape(title))
        .replace("__SLUG__", html.escape(slug))
        .replace("__BODY__", body)
    )


def build(*, check_only: bool = False) -> int:
    sources = sorted(p for p in SRC_DIR.glob("*.md") if p.name != "README.md")
    if not sources:
        print("no guides found in", SRC_DIR, file=sys.stderr)
        return 1

    template = load_template()
    entries: list[tuple[str, str]] = []
    for src in sources:
        # Source filenames are UPPER (matches the original docs/UPPER_LEARNING_GUIDE.html
        # convention); URLs and slugs in the rendered HTML stay lowercase for
        # readability.
        slug = src.stem.lower()
        md_text = src.read_text(encoding="utf-8")
        try:
            title, body = render_doc(md_text, slug)
        except ValueError as exc:
            print(f"{src}: {exc}", file=sys.stderr)
            return 1
        entries.append((slug, title))
        if check_only:
            continue
        OUT_DIR.mkdir(parents=True, exist_ok=True)
        (OUT_DIR / f"{slug}.html").write_text(
            wrap(template, title=title, slug=slug, body=body),
            encoding="utf-8",
        )

    if check_only:
        print(f"OK: {len(entries)} guide(s) parsed")
        return 0

    title, body = render_index(entries)
    (OUT_DIR / "index.html").write_text(
        wrap(template, title=title, slug="index", body=body),
        encoding="utf-8",
    )
    for asset in STATIC_ASSETS:
        shutil.copy2(ROOT / asset, OUT_DIR / asset)

    print(f"built {len(entries)} guide(s) → {OUT_DIR}")
    return 0


def main(argv: list[str] | None = None) -> int:
    summary = (__doc__ or "").splitlines()[0] if __doc__ else ""
    parser = argparse.ArgumentParser(description=summary)
    parser.add_argument(
        "--check",
        action="store_true",
        help="parse sources and validate quiz blocks without writing output",
    )
    args = parser.parse_args(argv)
    return build(check_only=args.check)


if __name__ == "__main__":
    raise SystemExit(main())
