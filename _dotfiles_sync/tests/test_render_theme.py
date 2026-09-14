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

# Deliberately not real Catppuccin values: the audit scan flags live
# palette hex outside managed regions, and this file is not a consumer.
PALETTE = {"mocha": {"base": "#abcdef", "mauve": "#fedcba"}}


class MarkerMatchTests(unittest.TestCase):
    """The marker must be the whole comment, not a phrase inside a line."""

    def test_real_marker_shapes_match(self) -> None:
        for line in (
            "# THEME BEGIN: sway-palette",
            "/* THEME BEGIN: waybar-palette */",
            "\t\t\t\t\t// THEME BEGIN: waybar-calendar-colors",
            "-- THEME BEGIN: nvim-starter-glyphs",
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
accent = "#fedcba"
bare = "abcdef"
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


class AuditTests(RendererFixture):
    """The blindspot backstop for the hand-maintained CONSUMERS list."""

    def setUp(self) -> None:
        super().setUp()
        self.known = render_theme.palette_values(PALETTE)

    def scan(self, body: str) -> list[tuple[int, str]]:
        path = self.root / "scanned"
        path.write_text(body, encoding="utf-8")
        return render_theme.unmanaged_palette_hex(path, self.known)

    def test_palette_hex_outside_a_region_is_reported(self) -> None:
        self.assertEqual(self.scan('fill = "#abcdef"\n'), [(1, "#abcdef")])

    def test_palette_hex_inside_a_region_is_managed(self) -> None:
        self.assertEqual(
            self.scan('# THEME BEGIN: demo\nfill = "#abcdef"\n# THEME END: demo\n'),
            [],
        )

    def test_non_palette_hex_is_not_drift(self) -> None:
        # guides/style.css:55 picks #eef2ff deliberately; it is not
        # Catppuccin, so the audit must leave it alone.
        self.assertEqual(self.scan("color: #eef2ff;\n"), [])


class LiveRepoAuditTests(unittest.TestCase):
    def test_repo_has_no_unmanaged_palette_hex(self) -> None:
        with contextlib.redirect_stdout(io.StringIO()) as out:
            code = render_theme.audit(render_theme.load_palette())
        self.assertEqual(code, 0, out.getvalue())


class GlyphVocabularyTests(unittest.TestCase):
    """docs/glyphs.toml is the one source for every glyph the repo draws.

    Glyphs are not colors, so they live beside the palette rather than in
    it, and they are data rather than Python so that surfaces which cannot
    import Python (.tmux.conf, CSS, INI) can be generated from the same
    table. These pin the file's shape and the keys downstream templates
    reference, so a rename or a typo fails the gate instead of rendering
    a tofu box on the status bar.
    """

    def write_glyphs(self, body: str) -> Path:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        path = Path(tmp.name) / "glyphs.toml"
        path.write_text(body, encoding="utf-8")
        return path

    def test_one_glyph_table_of_strings_loads(self) -> None:
        path = self.write_glyphs('[glyph]\nok = "\uf058"\nfail = "\uf057"\n')
        self.assertEqual(
            render_theme.load_glyphs(path), {"ok": "\uf058", "fail": "\uf057"}
        )

    def test_missing_glyph_table_fails_loud(self) -> None:
        path = self.write_glyphs('[mocha]\nbase = "#abcdef"\n')
        with self.assertRaises(SystemExit) as caught:
            render_theme.load_glyphs(path)
        self.assertIn("glyph", str(caught.exception))

    def test_non_table_glyph_fails_loud(self) -> None:
        path = self.write_glyphs('glyph = "\uf058"\n')
        with self.assertRaises(SystemExit) as caught:
            render_theme.load_glyphs(path)
        self.assertIn("glyph", str(caught.exception))

    def test_extra_table_fails_loud(self) -> None:
        path = self.write_glyphs('[glyph]\nok = "\uf058"\n\n[mocha]\nbase = "x"\n')
        with self.assertRaises(SystemExit) as caught:
            render_theme.load_glyphs(path)
        self.assertIn("mocha", str(caught.exception))

    def test_non_string_value_fails_loud(self) -> None:
        path = self.write_glyphs("[glyph]\nok = 3\n")
        with self.assertRaises(SystemExit) as caught:
            render_theme.load_glyphs(path)
        self.assertIn("ok", str(caught.exception))

    def test_palette_glyph_group_comes_from_the_toml(self) -> None:
        """The group is the file's content, not an import of _tmux_common."""
        self.assertEqual(
            render_theme.load_palette()["glyph"],
            render_theme.load_glyphs(render_theme.GLYPHS_PATH),
        )

    def test_palette_toml_may_not_shadow_the_glyph_group(self) -> None:
        path = self.write_glyphs('[glyph]\nok = "x"\n')
        with self.assertRaises(SystemExit) as caught:
            render_theme.load_palette(path)
        self.assertIn("reserved", str(caught.exception))

    def test_agent_state_glyphs_are_the_agreed_shapes(self) -> None:
        glyphs = render_theme.load_glyphs()
        self.assertEqual(
            {
                key: glyphs[key]
                for key in (
                    "agent_crashed",
                    "agent_blocked",
                    "agent_done",
                    "agent_working",
                    "agent_idle",
                )
            },
            {
                "agent_crashed": "\uf057",
                "agent_blocked": "\uf075",
                "agent_done": "\uf058",
                "agent_working": "\uf04b",
                "agent_idle": "\uf186",
            },
        )

    def test_cpu_and_ram_are_distinguishable(self) -> None:
        """They sit side by side on the Waybar bar; one shape reads as a bug."""
        glyphs = render_theme.load_glyphs()
        self.assertNotEqual(glyphs["cpu"], glyphs["ram"])

    def test_every_glyph_is_a_single_codepoint(self) -> None:
        for key, value in render_theme.load_glyphs().items():
            with self.subTest(key=key):
                self.assertEqual(len(value), 1, f"{key} is not one codepoint")

    def test_rendered_chain_carries_every_agent_glyph(self) -> None:
        glyphs = render_theme.load_glyphs()
        rendered = render_theme.render_region(
            "tmux-agent-glyphs", render_theme.load_palette()
        )
        for state in ("crashed", "blocked", "done", "working", "idle"):
            self.assertIn(
                glyphs[f"agent_{state}"],
                rendered,
                f"{state} glyph missing from the chain",
            )

    def test_changing_a_glyph_makes_the_conf_drift(self) -> None:
        """The whole point: edit the vocabulary, and check-theme must object."""
        palette = render_theme.load_palette()
        palette["glyph"] = dict(palette["glyph"], agent_crashed="\u2718")
        with contextlib.redirect_stdout(io.StringIO()) as out:
            code = render_theme.process(render_theme.CONSUMERS, palette, check=True)
        self.assertEqual(code, 1)
        self.assertIn("tmux.conf", out.getvalue())


# Files whose marker lines are test fixtures rather than live regions.
# This file builds STALE/RENDERED out of literal marker lines, so a repo
# scan sees them as real markers. Keyed by path with the reason so the
# exemption cannot quietly grow.
MARKER_FIXTURE_FILES: dict[str, str] = {
    "_dotfiles_sync/tests/test_render_theme.py": "marker fixtures for these tests",
}


def marker_bearing_files() -> dict[Path, list[str]]:
    """Every repo file carrying THEME markers, mapped to its region names."""
    found: dict[Path, list[str]] = {}
    for path in render_theme.iter_audit_files():
        try:
            text = path.read_text(encoding="utf-8")
        except (OSError, UnicodeDecodeError):
            continue
        names = [
            match.group("name")
            for line in text.splitlines()
            if (match := render_theme.MARKER_BEGIN_RE.match(line)) is not None
        ]
        if names:
            found[path] = names
    return found


class ConsumerRegistryTests(unittest.TestCase):
    """CONSUMERS is hand-maintained, so an unregistered region is invisible.

    `find_regions()` fails loud on a *registered* path whose markers do not
    match, but a marked file missing from the registry is simply never
    rendered: it keeps whatever bytes someone last typed, and `make theme`
    stays green. That is the silent-wrong-answer this enumerates away.
    """

    def test_every_marked_file_is_registered_with_its_regions(self) -> None:
        registered = {c.path: sorted(c.regions) for c in render_theme.CONSUMERS}
        for path, names in marker_bearing_files().items():
            rel = path.relative_to(render_theme.REPO_ROOT).as_posix()
            if rel in MARKER_FIXTURE_FILES:
                continue
            with self.subTest(path=rel):
                self.assertEqual(
                    registered.get(path),
                    sorted(names),
                    f"{rel} carries {sorted(names)} but CONSUMERS has "
                    f"{registered.get(path)}",
                )

    def test_registry_has_one_entry_per_path(self) -> None:
        paths = [c.path for c in render_theme.CONSUMERS]
        self.assertEqual(len(paths), len(set(paths)))

    def test_every_registered_region_has_a_template(self) -> None:
        for consumer in render_theme.CONSUMERS:
            for name in consumer.regions:
                with self.subTest(region=name):
                    self.assertTrue(
                        (render_theme.TEMPLATES_DIR / f"{name}.tmpl").is_file()
                    )


class GeneratedGlyphRegionTests(unittest.TestCase):
    """Migrated glyph regions must hold the vocabulary's bytes, not a copy.

    Each of these files used to spell its shapes inline. The check is that
    the live bytes equal what the template renders today, so a hand-edit
    inside the region (or a stale shape left behind by a migration) fails
    here as well as in `make check-theme`.
    """

    GLYPH_REGIONS = (
        "fuzzel-media-glyphs",
        "fuzzel-power-glyphs",
        "nvim-starter-glyphs",
        "pi-glyphs",
        "solo-glyphs",
        "tmux-agent-glyphs",
        "tmux-state-glyphs",
        "waybar-caffeinate-glyph",
        "waybar-issue-glyphs",
        "waybar-notification-glyphs",
        "zsh-prompt-glyphs",
    )

    def test_every_glyph_region_is_registered(self) -> None:
        registered = {n for c in render_theme.CONSUMERS for n in c.regions}
        for name in self.GLYPH_REGIONS:
            with self.subTest(region=name):
                self.assertIn(name, registered)

    def test_glyph_regions_render_only_vocabulary_values(self) -> None:
        palette = render_theme.load_palette()
        vocabulary = set(palette["glyph"].values())
        for name in self.GLYPH_REGIONS:
            rendered = render_theme.render_region(name, palette)
            drawn = {ch for ch in rendered if is_private_use(ch)}
            with self.subTest(region=name):
                self.assertTrue(drawn, f"{name} renders no glyph at all")
                self.assertEqual(drawn - vocabulary, set())

    def test_repo_is_in_sync_with_every_template(self) -> None:
        with contextlib.redirect_stdout(io.StringIO()) as out:
            code = render_theme.process(
                render_theme.CONSUMERS, render_theme.load_palette(), check=True
            )
        self.assertEqual(code, 0, out.getvalue())


def is_private_use(ch: str) -> bool:
    """Nerd Font shapes live in the Unicode private use areas."""
    return 0xE000 <= ord(ch) <= 0xF8FF or 0xF0000 <= ord(ch) <= 0xFFFFD


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
        self.assertEqual(render_theme.FILTERS["nohash"]("#abcdef"), "abcdef")
        self.assertEqual(render_theme.FILTERS["nohash"]("abcdef"), "abcdef")


if __name__ == "__main__":
    unittest.main()
