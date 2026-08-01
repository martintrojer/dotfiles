#!/usr/bin/env python3
"""Behavior tests for the guide renderer.

Run: python3 -m unittest discover -s guides -p 'test_*.py'
(`make check-guides` only parses the real guides; these cover the inline
renderer's edge cases that no current guide happens to exercise.)
"""

from __future__ import annotations

import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import build


class RenderInlineTests(unittest.TestCase):
    def test_emphasis_markers_inside_code_spans_stay_literal(self) -> None:
        self.assertEqual(
            build.render_inline("Match `*.ts` and `_foo_`, then *emphasis* here."),
            "Match <code>*.ts</code> and <code>_foo_</code>, "
            "then <em>emphasis</em> here.",
        )

    def test_real_emphasis_still_renders(self) -> None:
        self.assertEqual(
            build.render_inline("**bold** and *italic* and plain"),
            "<strong>bold</strong> and <em>italic</em> and plain",
        )

    def test_code_span_inside_link_text_survives(self) -> None:
        self.assertEqual(
            build.render_inline("[the `ls` page](https://example.com)"),
            '<a href="https://example.com">the <code>ls</code> page</a>',
        )

    def test_code_span_contents_are_html_escaped(self) -> None:
        self.assertEqual(
            build.render_inline("`<div> & co`"),
            "<code>&lt;div&gt; &amp; co</code>",
        )

    def test_placeholder_char_in_source_cannot_forge_a_code_span(self) -> None:
        self.assertEqual(build.render_inline("a\x000\x00b `x`"), "a0b <code>x</code>")


class WellFormednessTests(unittest.TestCase):
    """`--check` uses this; it is the only thing standing between a broken
    inline transform and a silently-shipped guide."""

    def assert_rejects(self, body: str, fragment: str) -> None:
        with self.assertRaises(ValueError) as caught:
            build.check_well_formed(body, "demo")
        self.assertIn(fragment, str(caught.exception))

    def test_overlapping_tags_are_rejected(self) -> None:
        self.assert_rejects("<p><em>a<code>b</em>c</code></p>", "</em> closes <code>")

    def test_unclosed_tag_is_rejected(self) -> None:
        self.assert_rejects("<ul>\n  <li>a</li>", "<ul> is never closed")

    def test_stray_end_tag_is_rejected(self) -> None:
        self.assert_rejects("<p>a</p></ul>", "</ul> with no open tag")

    def test_void_elements_need_no_close(self) -> None:
        build.check_well_formed('<label><input type="radio"> a</label>', "demo")

    def test_real_rendered_blocks_pass(self) -> None:
        body = build.render_block(
            "# Title\n\nSome *emphasis* and `code` here.\n\n- one\n- two\n"
        )
        build.check_well_formed(body, "demo")

    def test_rendered_quiz_passes(self) -> None:
        body = build.render_quiz(
            '[[questions]]\nq = "hi"\noptions = ["a", "b"]\nanswer = 1\n', "demo"
        )
        build.check_well_formed(body, "demo")

    def test_every_shipped_guide_renders_well_formed(self) -> None:
        sources = sorted(p for p in build.SRC_DIR.glob("*.md") if p.name != "README.md")
        self.assertTrue(sources)
        for src in sources:
            with self.subTest(guide=src.name):
                _, body = build.render_doc(
                    src.read_text(encoding="utf-8"), src.stem.lower()
                )
                build.check_well_formed(body, src.name)


class RenderBlockTests(unittest.TestCase):
    def test_bullets_collect_into_one_list(self) -> None:
        self.assertEqual(
            build.render_block("- one\n- two\n"),
            "<ul>\n  <li>one</li>\n  <li>two</li>\n</ul>",
        )

    def test_heading_level_follows_hash_count(self) -> None:
        self.assertEqual(build.render_block("### deep"), "<h3>deep</h3>")

    def test_paragraph_lines_join_with_a_space(self) -> None:
        self.assertEqual(build.render_block("one\ntwo"), "<p>one two</p>")


class RenderQuizTests(unittest.TestCase):
    def assert_message(self, body: str, expected: str) -> None:
        with self.assertRaises(ValueError) as caught:
            build.render_quiz(body, "demo")
        self.assertEqual(str(caught.exception), expected)

    def test_missing_keys_report_guide_and_index(self) -> None:
        self.assert_message(
            '[[questions]]\noptions = ["a", "b"]\nanswer = 0\n',
            "quiz 'demo' question 0: missing 'q'",
        )
        self.assert_message(
            '[[questions]]\nq = "hi"\nanswer = 0\n',
            "quiz 'demo' question 0: missing 'options'",
        )
        self.assert_message(
            '[[questions]]\nq = "hi"\noptions = ["a", "b"]\n',
            "quiz 'demo' question 0: missing 'answer'",
        )

    def test_non_numeric_answer_reports_guide_and_index(self) -> None:
        self.assert_message(
            '[[questions]]\nq = "hi"\noptions = ["a", "b"]\nanswer = "nope"\n',
            "quiz 'demo' question 0: answer must be an integer, got 'nope'",
        )

    def test_valid_quiz_renders(self) -> None:
        html_out = build.render_quiz(
            '[[questions]]\nq = "hi"\noptions = ["a", "b"]\nanswer = 1\n', "demo"
        )
        self.assertIn('data-answer="1"', html_out)
        self.assertIn("<legend>hi</legend>", html_out)


if __name__ == "__main__":
    unittest.main()
