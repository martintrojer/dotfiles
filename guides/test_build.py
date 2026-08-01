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
