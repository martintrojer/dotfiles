#!/usr/bin/env python3
"""Regression tests for window-back-and-forth's focus-history lookup.

Scope per docs/DECISIONS.md ("No unit tests for the control-plane and helper
scripts" + its 2026-08-01 amendment): only the lookup, which fails by landing
on the wrong window -- something you notice by feel, not by an error. The
swaymsg call and arg handling fail visibly and stay untested.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import types
import unittest
from pathlib import Path

SCRIPTS = Path(__file__).resolve().parent.parent


def load_script(name: str, path: Path) -> types.ModuleType:
    """Import a colocated script by path (they have no .py extension)."""
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


wbf = load_script("window_back_and_forth", SCRIPTS / "window-back-and-forth")


class BackAndForth(unittest.TestCase):
    """The history lookup behind mod+Shift+g.

    Silent when wrong, so both the ordering and the split-container descent
    are pinned here.
    """

    def win(self, cid: int, **kw) -> dict:
        return {"id": cid, "type": "con", **kw}

    def test_the_previous_window_is_focus_index_one(self) -> None:
        # focus[0] is where you are; focus[1] is where you were.
        ws = {
            "type": "workspace",
            "focus": [8, 7],
            "floating_nodes": [self.win(7), self.win(8)],
        }
        self.assertEqual(wbf.other_window_id(ws), 7)

    def test_swapping_twice_returns_you_home(self) -> None:
        # Sway rewrites the array on each focus, so the second press sees it
        # reversed. That round trip is the whole contract.
        first = {
            "type": "workspace",
            "focus": [8, 7],
            "floating_nodes": [self.win(7), self.win(8)],
        }
        self.assertEqual(wbf.other_window_id(first), 7)
        after = {
            "type": "workspace",
            "focus": [7, 8],
            "floating_nodes": [self.win(7), self.win(8)],
        }
        self.assertEqual(wbf.other_window_id(after), 8)

    def test_a_split_container_resolves_to_a_real_window(self) -> None:
        # Measured on a live tree: focus[1] can name an unnamed split
        # container, whose own focus[0] is the leaf we want.
        ws = {
            "type": "workspace",
            "focus": [5, 89],
            "nodes": [
                self.win(5),
                {
                    "id": 89,
                    "type": "con",
                    "focus": [90, 88],
                    "nodes": [self.win(88), self.win(90)],
                },
            ],
        }
        self.assertEqual(wbf.other_window_id(ws), 90)

    def test_a_lone_window_has_nothing_to_swap_to(self) -> None:
        ws = {"type": "workspace", "focus": [5], "nodes": [self.win(5)]}
        self.assertIsNone(wbf.other_window_id(ws))

    def test_an_empty_workspace_is_none(self) -> None:
        self.assertIsNone(wbf.other_window_id({"type": "workspace", "focus": []}))

    def test_stale_focus_entries_are_skipped(self) -> None:
        # A closed window lingers in the focus array; naming its con_id would
        # make the keypress a silent no-op.
        ws = {
            "type": "workspace",
            "focus": [8, 999, 7],
            "floating_nodes": [self.win(7), self.win(8)],
        }
        self.assertEqual(wbf.other_window_id(ws), 7)

    def test_a_cyclic_tree_does_not_hang_the_keybinding(self) -> None:
        loop: dict = {"id": 1, "type": "con", "focus": [2]}
        child: dict = {"id": 2, "type": "con", "focus": [1], "nodes": [loop]}
        loop["nodes"] = [child]
        self.assertIsNone(wbf.leaf_of(loop, {1: loop, 2: child}))
