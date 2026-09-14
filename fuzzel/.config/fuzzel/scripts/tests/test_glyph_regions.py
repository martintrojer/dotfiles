#!/usr/bin/env python3
"""Focused regression tests for the fuzzel pickers' glyph constants.

Scope per docs/DECISIONS.md ("No unit tests for the control-plane and helper
scripts" + its 2026-08-01 amendment): these menu labels are now *generated*
into marker-bounded regions from ``docs/glyphs.toml``, so a wrong-key
substitution is a silent wrong answer -- the row still shows a plausible
glyph, just the wrong one, and "Shutdown" wearing the reboot shape is a
mis-click waiting to happen. The row *labels* also key the action lookup, so
a stale label silently makes a row do nothing.

Not covered: the playerctl/systemctl plumbing behind each row, which fails
visibly.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import sys
import tomllib
import types
import unittest
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent
REPO_ROOT = SCRIPTS.parents[3]
GLYPHS_PATH = REPO_ROOT / "docs" / "glyphs.toml"


def load_script(name: str, path: Path) -> types.ModuleType:
    """Import a colocated script by path (they have no .py extension)."""
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


sys.path.insert(0, str(SCRIPTS))
powermenu = load_script("fuzzel_powermenu_under_test", SCRIPTS / "powermenu")
cider = load_script("fuzzel_cider_under_test", SCRIPTS / "cider")

with GLYPHS_PATH.open("rb") as fp:
    GLYPHS: dict[str, str] = tomllib.load(fp)["glyph"]


def is_private_use(char: str) -> bool:
    code = ord(char)
    return 0xE000 <= code <= 0xF8FF or 0xF0000 <= code <= 0xFFFFD


def inline_glyphs(path: Path) -> set[str]:
    """Private use codepoints in `path` that sit outside a THEME region."""
    found: set[str] = set()
    inside = False
    for line in path.read_text(encoding="utf-8").splitlines():
        stripped = line.lstrip()
        if stripped.startswith("# THEME BEGIN:"):
            inside = True
            continue
        if stripped.startswith("# THEME END:"):
            inside = False
            continue
        if not inside:
            found.update(ch for ch in line if is_private_use(ch))
    return found


class GeneratedGlyphConstants(unittest.TestCase):
    def test_power_constants_come_from_the_vocabulary(self) -> None:
        for attr, key in (
            ("ICON_LOCK", "power_lock"),
            ("ICON_LOGOUT", "power_logout"),
            ("ICON_REBOOT", "power_reboot"),
            ("ICON_SHUTDOWN", "power_shutdown"),
            ("ICON_SUSPEND", "power_suspend"),
            ("ICON_CAFFEINATE", "caffeinate"),
        ):
            with self.subTest(constant=attr):
                self.assertEqual(getattr(powermenu, attr), GLYPHS[key])

    def test_media_constants_come_from_the_vocabulary(self) -> None:
        for attr, key in (
            ("ICON_PLAY", "media_play"),
            ("ICON_NEXT", "media_next"),
            ("ICON_PREVIOUS", "media_previous"),
        ):
            with self.subTest(constant=attr):
                self.assertEqual(getattr(cider, attr), GLYPHS[key])

    def test_destructive_power_rows_never_share_a_shape(self) -> None:
        shapes = [
            powermenu.ICON_LOCK,
            powermenu.ICON_LOGOUT,
            powermenu.ICON_REBOOT,
            powermenu.ICON_SHUTDOWN,
            powermenu.ICON_SUSPEND,
        ]
        self.assertEqual(len(set(shapes)), len(shapes))

    def test_only_the_known_local_glyph_is_spelled_outside_a_region(self) -> None:
        # Generation is pointless if a shape can still be spelled inline, so
        # scan for Nerd Font codepoints (private use areas) outside the
        # generated regions and pin the survivors.
        #
        # `ICON_SHOW` stays local: "show the app window" is a Cider-specific
        # affordance, not a shared meaning, and the vocabulary has no key for
        # it. chrome-tabs' ○●◌◍ are structural instance markers outside the
        # private use areas, so they are excluded by construction rather than
        # by an exemption list.
        allowed = {
            "powermenu": set(),
            "cider": {cider.ICON_SHOW},
        }
        for path in (SCRIPTS / "powermenu", SCRIPTS / "cider"):
            with self.subTest(file=path.name):
                self.assertEqual(
                    inline_glyphs(path),
                    allowed[path.name],
                    f"inline glyph in {path.name}",
                )


class RowLabelsUseTheConstants(unittest.TestCase):
    def test_power_rows_are_labelled_with_their_constants(self) -> None:
        expected = {
            "lock": powermenu.ICON_LOCK,
            "reboot": powermenu.ICON_REBOOT,
            "logout": powermenu.ICON_LOGOUT,
            "shutdown": powermenu.ICON_SHUTDOWN,
            "suspend": powermenu.ICON_SUSPEND,
            "caffeinate": powermenu.ICON_CAFFEINATE,
        }
        options = powermenu.power_options(caffeinate_on=False)
        self.assertEqual([action for _, action in options], list(expected))
        for label, action in options:
            with self.subTest(action=action):
                self.assertTrue(label.startswith(expected[action]), repr(label))

    def test_the_caffeinate_row_reports_its_state(self) -> None:
        off = {action: label for label, action in powermenu.power_options(False)}
        on = {action: label for label, action in powermenu.power_options(True)}
        self.assertEqual(off["caffeinate"], f"{powermenu.ICON_CAFFEINATE} Caffeinate")
        self.assertEqual(
            on["caffeinate"], f"{powermenu.ICON_CAFFEINATE} Caffeinate: on"
        )

    def test_cider_rows_are_labelled_with_their_constants(self) -> None:
        expected = {
            "play-pause": cider.ICON_PLAY,
            "next": cider.ICON_NEXT,
            "previous": cider.ICON_PREVIOUS,
            "show": cider.ICON_SHOW,
        }
        self.assertEqual([action for _, action in cider.ACTIONS], list(expected))
        for label, action in cider.ACTIONS:
            with self.subTest(action=action):
                self.assertTrue(label.startswith(expected[action]), repr(label))


if __name__ == "__main__":
    unittest.main()
