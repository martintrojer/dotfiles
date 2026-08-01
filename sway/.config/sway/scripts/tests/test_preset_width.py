#!/usr/bin/env python3
"""Focused regression tests for the preset-width sway tree walk.

Scope per docs/DECISIONS.md ("No unit tests for the control-plane and helper
scripts" + its 2026-08-01 amendment): only the two functions that shipped
crashes on a normal keypress -- ``is_floating`` against a workspace node, and
``get_focused``'s walk over a malformed or empty tree. The preset arithmetic
and the state-file counter are not covered: both fail visibly and immediately.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import subprocess
import sys
import types
import unittest
from pathlib import Path
from unittest import mock

SCRIPTS = Path(__file__).resolve().parent.parent


def load_script(name: str, path: Path) -> types.ModuleType:
    """Import a colocated script by path (they have no .py extension)."""
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


preset = load_script("preset_width_under_test", SCRIPTS / "preset-width")

RECT = {"x": 0, "y": 0, "width": 2560, "height": 1440}


class IsFloating(unittest.TestCase):
    def test_workspace_node_is_not_floating(self) -> None:
        # An empty workspace is marked focused by sway and carries
        # "floating": null. dict.get(..., "") does not substitute for an
        # explicit None, so this used to raise AttributeError on .startswith
        # every time the binding was pressed.
        self.assertFalse(preset.is_floating({"type": "workspace", "floating": None}))

    def test_user_floated_window_is_floating(self) -> None:
        self.assertTrue(preset.is_floating({"type": "con", "floating": "user_on"}))
        self.assertTrue(preset.is_floating({"type": "floating_con"}))

    def test_tiled_window_is_not_floating(self) -> None:
        self.assertFalse(preset.is_floating({"type": "con", "floating": "auto_off"}))
        self.assertFalse(preset.is_floating({"type": "con"}))


class GetFocused(unittest.TestCase):
    def walk(self, raw: str, returncode: int = 0):
        def fake_swaymsg(*args: str) -> str:
            if returncode:
                raise subprocess.CalledProcessError(returncode, ["swaymsg", *args])
            return raw

        with mock.patch.object(preset, "swaymsg", fake_swaymsg):
            return preset.get_focused()

    def tree(self, ws_nodes: list, *, ws_focused: bool = False) -> str:
        return json.dumps(
            {
                "type": "root",
                "name": None,  # the root's name really is null
                "nodes": [
                    {
                        "type": "output",
                        "name": "DP-1",
                        "rect": RECT,
                        "nodes": [
                            {
                                "type": "workspace",
                                "name": "1",
                                "rect": RECT,
                                "focused": ws_focused,
                                "nodes": ws_nodes,
                            }
                        ],
                    }
                ],
            }
        )

    def test_focused_window_carries_its_output_and_workspace(self) -> None:
        win = {"type": "con", "name": "term", "focused": True, "floating": "auto_off"}
        result = self.walk(self.tree([win]))
        assert result is not None
        output, workspace, container = result
        self.assertEqual(output["name"], "DP-1")
        self.assertEqual(workspace["name"], "1")
        self.assertEqual(container["name"], "term")

    def test_empty_workspace_returns_the_workspace_itself(self) -> None:
        result = self.walk(self.tree([], ws_focused=True))
        assert result is not None
        self.assertEqual(result[2]["type"], "workspace")

    def test_nothing_focused_anywhere_is_none(self) -> None:
        win = {"type": "con", "name": "term", "focused": False}
        self.assertIsNone(self.walk(self.tree([win])))

    def test_scratchpad_output_is_not_reported_as_the_output(self) -> None:
        raw = json.dumps(
            {
                "type": "root",
                "name": None,
                "nodes": [
                    {
                        "type": "output",
                        "name": "__i3",
                        "rect": RECT,
                        "nodes": [
                            {"type": "con", "name": "hidden", "focused": True},
                        ],
                    }
                ],
            }
        )
        result = self.walk(raw)
        assert result is not None
        self.assertIsNone(result[0])

    def test_non_dict_children_do_not_crash_the_walk(self) -> None:
        win = {"type": "con", "name": "term", "focused": True}
        self.assertIsNotNone(self.walk(self.tree([None, "junk", win])))

    def test_swaymsg_failure_and_junk_output_are_none_not_tracebacks(self) -> None:
        self.assertIsNone(self.walk("{}", returncode=1))
        self.assertIsNone(self.walk("not json"))
        self.assertIsNone(self.walk("[1, 2]"))


if __name__ == "__main__":
    unittest.main()
