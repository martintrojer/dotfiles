#!/usr/bin/env python3
"""Focused regression tests for the waybar scripts' glyph constants.

Scope per docs/DECISIONS.md ("No unit tests for the control-plane and helper
scripts" + its 2026-08-01 amendment): the glyphs in these modules are now
*generated* into marker-bounded regions from ``docs/glyphs.toml``, so a
wrong-key substitution is a silent wrong answer -- the bar still renders a
plausible glyph, just the wrong one (a thermometer reading as a harddisk, or
CPU and RAM collapsing to the same shape side by side). That is invisible
without a patched font and exactly what a test can catch.

What is *not* covered: the sensor readers, the mako plumbing and the payload
classes. Those fail visibly in the bar.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import io
import json
import sys
import tomllib
import types
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock

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
caffeinate = load_script("waybar_caffeinate_under_test", SCRIPTS / "caffeinate")
issues = load_script("waybar_issues_under_test", SCRIPTS / "issues")
notifications = load_script(
    "waybar_notifications_under_test", SCRIPTS / "notifications"
)

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
    """Each local constant must carry its vocabulary *meaning*, not any shape."""

    def test_issue_constants_come_from_the_vocabulary(self) -> None:
        for attr, key in (
            ("ICON_CPU", "cpu"),
            ("ICON_TEMP", "temperature"),
            ("ICON_RAM", "ram"),
            ("ICON_SWAP", "swap"),
            ("ICON_DISK", "disk"),
            ("ICON_NET", "network"),
            ("ICON_AUDIO", "audio"),
            ("ICON_MIC", "microphone"),
            ("ICON_BT", "bluetooth"),
        ):
            with self.subTest(constant=attr):
                self.assertEqual(getattr(issues, attr), GLYPHS[key])

    def test_temperature_is_the_nf_md_thermometer(self) -> None:
        # The one shape that deliberately changed with the vocabulary: the
        # hardware group reads as a set only if every glyph is nf-md, so
        # temperature left nf-fa U+F2C9 for md-thermometer.
        self.assertEqual(issues.ICON_TEMP, "\U000f050f")

    def test_cpu_and_ram_stay_distinguishable(self) -> None:
        # They render side by side on the bar; one wrong key makes the issues
        # segment read "two of the same thing".
        self.assertNotEqual(issues.ICON_CPU, issues.ICON_RAM)

    def test_caffeinate_and_notification_constants_come_from_the_vocabulary(
        self,
    ) -> None:
        self.assertEqual(caffeinate.ICON, GLYPHS["caffeinate"])
        self.assertEqual(notifications.ICON_BELL, GLYPHS["notifications_on"])
        self.assertEqual(notifications.ICON_BELL_OFF, GLYPHS["notifications_off"])

    def test_no_waybar_script_inlines_a_private_use_glyph(self) -> None:
        # Generation is pointless if a shape can still be spelled inline. Nerd
        # Font glyphs live in the private use areas, so scan for those outside
        # the generated regions.
        for name in ("caffeinate", "issues", "notifications"):
            with self.subTest(file=name):
                self.assertEqual(
                    inline_glyphs(SCRIPTS / name), set(), f"inline glyph in {name}"
                )


class RenderedLabelsUseTheConstants(unittest.TestCase):
    """The payloads must be built from the constants, not a second copy."""

    def test_caffeinate_on_renders_its_icon(self) -> None:
        buf = io.StringIO()
        with (
            mock.patch.object(caffeinate, "flag_path", lambda: Path("/proc/self")),
            redirect_stdout(buf),
        ):
            caffeinate.render()
        self.assertEqual(json.loads(buf.getvalue())["text"], caffeinate.ICON)

    def test_every_demo_issue_label_starts_with_its_named_constant(self) -> None:
        expected = {
            "cpu": issues.ICON_CPU,
            "temperature": issues.ICON_TEMP,
            "memory": issues.ICON_RAM,
            "swap": issues.ICON_SWAP,
            "disk": issues.ICON_DISK,
            "network": issues.ICON_NET,
            "bluetooth": issues.ICON_BT,
            "audio": issues.ICON_AUDIO,
            "mic": issues.ICON_MIC,
        }
        demo = issues.demo_issues()
        self.assertEqual([issue.key for issue in demo], list(expected))
        for issue in demo:
            with self.subTest(key=issue.key):
                self.assertTrue(
                    issue.text.startswith(expected[issue.key]),
                    f"{issue.key}: {issue.text!r}",
                )

    def render_notifications(self, *, dnd: bool, count: int) -> dict:
        history = [{"app_name": "Probe", "summary": "s"} for _ in range(count)]
        buf = io.StringIO()
        with (
            mock.patch.object(notifications, "demo_enabled", lambda: False),
            mock.patch.object(
                notifications,
                "mako_json",
                lambda cmd: history if cmd == "history" else [],
            ),
            mock.patch.object(
                notifications,
                "mako_modes",
                lambda: {"do-not-disturb"} if dnd else set(),
            ),
            redirect_stdout(buf),
        ):
            notifications.waybar()
        return json.loads(buf.getvalue())

    def test_notification_text_uses_the_on_and_off_bells(self) -> None:
        self.assertEqual(
            self.render_notifications(dnd=False, count=2)["text"],
            f"{notifications.ICON_BELL} 2",
        )
        self.assertEqual(
            self.render_notifications(dnd=True, count=2)["text"],
            f"{notifications.ICON_BELL_OFF} 2",
        )
        self.assertEqual(
            self.render_notifications(dnd=True, count=0)["text"],
            notifications.ICON_BELL_OFF,
        )
        self.assertEqual(self.render_notifications(dnd=False, count=0)["text"], "")


if __name__ == "__main__":
    unittest.main()
