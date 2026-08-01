#!/usr/bin/env python3
"""Focused behavior tests for the theme renderer.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

`make check-theme` only compares the renderer against its own output, so
it cannot catch a bug in the renderer itself — and the renderer rewrites
16 live config files. These cover the splice math, the marker parser and
the drift check. Authorised by the 2026-08-01 amendment to "No unit tests
for the control-plane" in docs/DECISIONS.md.
"""

from __future__ import annotations

import contextlib
import io
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import render_theme

PALETTE = {"mocha": {"base": "#1e1e2e", "mauve": "#cba6f7"}}


class MarkerMatchTests(unittest.TestCase):
    """The marker must be the whole comment, not a phrase inside a line."""

    def test_real_marker_shapes_match(self) -> None:
        for line in (
            "# THEME BEGIN: sway-palette",
            "/* THEME BEGIN: waybar-palette */",
            "\t\t\t\t\t// THEME BEGIN: waybar-calendar-colors",
        ):
            with self.subTest(line=line):
                match = render_theme.MARKER_BEGIN_RE.match(line)
                self.assertIsNotNone(match)
                assert match is not None
                self.assertTrue(match.group("name"))

    def test_prose_mentioning_the_phrase_is_not_a_marker(self) -> None:
        for line in (
            "file that carries a `THEME BEGIN: name ... THEME END: name`",
            "# them into every config file with a THEME BEGIN: block",
            "echo 'THEME END: sway-palette'",
        ):
            with self.subTest(line=line):
                self.assertIsNone(render_theme.MARKER_BEGIN_RE.match(line))
                self.assertIsNone(render_theme.MARKER_END_RE.match(line))


class RendererFixture(unittest.TestCase):
    """A throwaway repo root with one template and one consumer file."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        templates = self.root / "themes"
        templates.mkdir()
        (templates / "demo.tmpl").write_text(
            'accent = "{{mocha.mauve}}"\nbare = "{{mocha.base|nohash}}"\n',
            encoding="utf-8",
        )
        patches = (
            mock.patch.object(render_theme, "REPO_ROOT", self.root),
            mock.patch.object(render_theme, "TEMPLATES_DIR", templates),
        )
        for patch in patches:
            patch.start()
            self.addCleanup(patch.stop)

    def consumer(self, body: str, name: str = "config") -> render_theme.Consumer:
        path = self.root / name
        path.write_text(body, encoding="utf-8")
        return render_theme.Consumer(path=path, regions=("demo",))

    def run_process(self, consumer: render_theme.Consumer, *, check: bool) -> int:
        with contextlib.redirect_stdout(io.StringIO()):
            return render_theme.process([consumer], PALETTE, check=check)


STALE = """keep me
# THEME BEGIN: demo
accent = "#000000"
# THEME END: demo
keep me too
"""

RENDERED = """keep me
# THEME BEGIN: demo
accent = "#cba6f7"
bare = "1e1e2e"
# THEME END: demo
keep me too
"""


class WriteTests(RendererFixture):
    def test_write_replaces_only_the_region_and_is_idempotent(self) -> None:
        consumer = self.consumer(STALE)
        self.assertEqual(self.run_process(consumer, check=False), 0)
        first = consumer.path.read_text(encoding="utf-8")
        self.assertEqual(first, RENDERED)

        self.assertEqual(self.run_process(consumer, check=False), 0)
        self.assertEqual(consumer.path.read_text(encoding="utf-8"), first)

    def test_prose_mentioning_a_marker_does_not_split_the_region(self) -> None:
        consumer = self.consumer(
            "# the block below is a THEME BEGIN: demo region, do not edit\n" + STALE
        )
        self.assertEqual(self.run_process(consumer, check=False), 0)
        self.assertEqual(
            consumer.path.read_text(encoding="utf-8"),
            "# the block below is a THEME BEGIN: demo region, do not edit\n" + RENDERED,
        )


class CheckTests(RendererFixture):
    def test_check_reports_drift_without_touching_the_file(self) -> None:
        consumer = self.consumer(STALE)
        with contextlib.redirect_stdout(io.StringIO()) as out:
            code = render_theme.process([consumer], PALETTE, check=True)
        self.assertEqual(code, 1)
        self.assertIn("DRIFT: config", out.getvalue())
        self.assertEqual(consumer.path.read_text(encoding="utf-8"), STALE)

    def test_check_is_quiet_once_in_sync(self) -> None:
        consumer = self.consumer(RENDERED)
        self.assertEqual(self.run_process(consumer, check=True), 0)


class MalformedMarkerTests(RendererFixture):
    def assert_refuses(self, body: str, expected_fragment: str) -> None:
        consumer = self.consumer(body)
        with self.assertRaises(SystemExit) as caught:
            self.run_process(consumer, check=False)
        self.assertIn(expected_fragment, str(caught.exception))
        # The file must survive a parse failure untouched.
        self.assertEqual(consumer.path.read_text(encoding="utf-8"), body)

    def test_missing_end_marker_refuses_to_write(self) -> None:
        self.assert_refuses(
            'keep me\n# THEME BEGIN: demo\naccent = "#000000"\n',
            "has no matching END",
        )

    def test_orphan_end_marker_refuses_to_write(self) -> None:
        self.assert_refuses("keep me\n# THEME END: demo\n", "with no matching BEGIN")

    def test_mismatched_marker_names_refuse_to_write(self) -> None:
        self.assert_refuses(
            "# THEME BEGIN: demo\n# THEME END: other\n",
            "does not match BEGIN",
        )


class TemplateExpansionTests(unittest.TestCase):
    def test_unknown_color_fails_loud(self) -> None:
        with self.assertRaises(SystemExit) as caught:
            render_theme.expand_template("{{mocha.nope}}", PALETTE, where="t")
        self.assertIn("unknown color", str(caught.exception))

    def test_unknown_filter_fails_loud(self) -> None:
        with self.assertRaises(SystemExit) as caught:
            render_theme.expand_template("{{mocha.base|nope}}", PALETTE, where="t")
        self.assertIn("unknown filter", str(caught.exception))

    def test_nohash_strips_only_the_leading_hash(self) -> None:
        self.assertEqual(render_theme.FILTERS["nohash"]("#1e1e2e"), "1e1e2e")
        self.assertEqual(render_theme.FILTERS["nohash"]("1e1e2e"), "1e1e2e")


if __name__ == "__main__":
    unittest.main()
