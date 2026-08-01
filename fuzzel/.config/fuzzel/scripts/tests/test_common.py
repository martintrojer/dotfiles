#!/usr/bin/env python3
"""Focused regression tests for the fuzzel picker helpers.

Scope is deliberately narrow (docs/DECISIONS.md, "No unit tests for the
control-plane and helper scripts" + its 2026-08-01 amendment): only the
helpers that shipped a bug whose failure mode is a *silent wrong answer* --
row selection, the sway tree walk feeding it, and XDG cache resolution.
Parsers that merely fail visibly (ssh/toolbox/hotkey list parsing) stay
untested on purpose.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
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


common = load_script("fuzzel_common_under_test", SCRIPTS / "_common.py")


def completed(stdout: str, returncode: int = 0) -> subprocess.CompletedProcess[str]:
    return subprocess.CompletedProcess([], returncode, stdout, "")


def window(con_id: int, title: str, *, focused: bool = False) -> dict:
    """A sway tree leaf. ``pid`` is what marks a con as a real window."""
    return {
        "id": con_id,
        "type": "con",
        "pid": 1000 + con_id,
        "name": title,
        "app_id": "term",
        "focused": focused,
    }


def workspace(ws_id: int, name: str, nodes: list[dict], focus: list[int]) -> dict:
    return {
        "id": ws_id,
        "type": "workspace",
        "name": name,
        "nodes": nodes,
        "focus": focus,
    }


class FuzzelIndexSelection(unittest.TestCase):
    """fuzzel prints the selected row number; the old code reverse-mapped the
    display *string*, so duplicate labels always resolved to the first match."""

    def select(self, stdout: str, returncode: int = 0, options=("a", "b", "c")):
        with mock.patch.object(
            common, "run", return_value=completed(stdout, returncode)
        ):
            return common.fuzzel_dmenu_index(
                prompt="p ", width=60, options=list(options)
            )

    def test_duplicate_labels_resolve_to_the_row_actually_picked(self) -> None:
        rows = ["same title", "other", "same title"]
        self.assertEqual(self.select("2\n", options=rows), 2)
        self.assertEqual(self.select("0\n", options=rows), 0)

    def test_cancel_is_distinct_from_free_text(self) -> None:
        # Escape / focus loss: nonzero rc, nothing selected.
        self.assertIsNone(self.select("", returncode=2))
        # Enter on text matching no row: fuzzel prints -1.
        self.assertEqual(self.select("-1\n"), -1)

    def test_out_of_range_and_garbage_never_index_a_row(self) -> None:
        self.assertEqual(self.select("9\n"), -1)
        self.assertIsNone(self.select("not a number\n"))

    def test_index_flag_is_passed_to_fuzzel(self) -> None:
        with mock.patch.object(
            common, "run", return_value=completed("0\n")
        ) as run_mock:
            common.fuzzel_dmenu_index(prompt="p ", width=60, options=["a"])
        self.assertIn("--index", run_mock.call_args.args[0])


class SwayTreeWalk(unittest.TestCase):
    def tree(self, outputs: list[dict], focus: list[int] | None = None) -> dict:
        return {
            "id": 1,
            "type": "root",
            "name": None,
            "nodes": outputs,
            "focus": focus or [o["id"] for o in outputs],
        }

    def output(self, out_id: int, name: str, wss: list[dict], focus: list[int]) -> dict:
        return {
            "id": out_id,
            "type": "output",
            "name": name,
            "nodes": wss,
            "focus": focus,
        }

    def walk(self, tree: dict, **kwargs) -> list[dict]:
        with mock.patch.object(common, "_get_sway_tree", return_value=tree):
            return common.list_sway_windows(**kwargs)

    def test_mru_round_robins_across_workspaces_instead_of_draining_one(self) -> None:
        # ws1 focus order [11, 12]; ws2 focus order [21, 22]. A depth-first
        # walk would emit 11,12,21,22 -- burying the last-used window of the
        # previously focused workspace behind an unrelated one.
        ws1 = workspace(10, "1", [window(11, "a"), window(12, "b")], [11, 12])
        ws2 = workspace(20, "2", [window(21, "c"), window(22, "d")], [21, 22])
        tree = self.tree([self.output(2, "DP-1", [ws1, ws2], [10, 20])])

        ids = [w["id"] for w in self.walk(tree, mru=True)]
        self.assertEqual(ids, [11, 21, 12, 22])

        dfs_ids = [w["id"] for w in self.walk(tree)]
        self.assertEqual(dfs_ids, [11, 12, 21, 22])

    def test_output_focus_order_decides_which_workspace_leads(self) -> None:
        ws1 = workspace(10, "1", [window(11, "a")], [11])
        ws2 = workspace(20, "2", [window(21, "c")], [21])
        # Same workspaces, output focus says ws2 was used most recently.
        tree = self.tree([self.output(2, "DP-1", [ws1, ws2], [20, 10])])
        self.assertEqual([w["id"] for w in self.walk(tree, mru=True)], [21, 11])

    def test_focused_window_is_pushed_last_so_row_zero_is_the_other_one(self) -> None:
        ws1 = workspace(
            10, "1", [window(11, "a", focused=True), window(12, "b")], [11, 12]
        )
        tree = self.tree([self.output(2, "DP-1", [ws1], [10])])
        self.assertEqual([w["id"] for w in self.walk(tree, mru=True)], [12, 11])

    def test_scratchpad_workspaces_are_skipped(self) -> None:
        scratch = workspace(90, "__i3_scratch", [window(91, "hidden")], [91])
        ws1 = workspace(10, "1", [window(11, "a")], [11])
        tree = self.tree([self.output(2, "DP-1", [scratch, ws1], [90, 10])])
        self.assertEqual([w["id"] for w in self.walk(tree, mru=True)], [11])

    def test_windows_not_in_the_focus_array_still_appear(self) -> None:
        # A freshly mapped window has no focus-history entry yet.
        ws1 = workspace(10, "1", [window(11, "a"), window(12, "new")], [11])
        tree = self.tree([self.output(2, "DP-1", [ws1], [10])])
        self.assertEqual([w["id"] for w in self.walk(tree, mru=True)], [11, 12])

    def test_ipc_failure_is_empty_by_default_and_loud_in_strict_mode(self) -> None:
        with mock.patch.object(common, "_get_sway_tree", return_value=None):
            self.assertEqual(common.list_sway_windows(), [])
            with self.assertRaises(common.ScriptError):
                common.list_sway_windows(strict=True)

    def test_malformed_swaymsg_output_yields_no_tree(self) -> None:
        for stdout, rc in (("not json", 0), ("[1, 2]", 0), ("{}", 1)):
            with (
                self.subTest(stdout=stdout, rc=rc),
                mock.patch.object(common, "run", return_value=completed(stdout, rc)),
            ):
                self.assertIsNone(common._get_sway_tree())


class PickerCachePath(unittest.TestCase):
    def test_empty_xdg_cache_home_falls_back_to_an_absolute_path(self) -> None:
        # Per the XDG spec an empty value means unset. The `,`-default spelling
        # returned "" and produced the relative path "fuzzel/pickers/x.cache",
        # which lands in whatever CWD sway happened to hand the script.
        with mock.patch.dict(os.environ, {"XDG_CACHE_HOME": ""}, clear=False):
            path = Path(common.picker_cache_path("windows"))
        self.assertTrue(path.is_absolute(), path)
        self.assertEqual(path.name, "windows.cache")

    def test_xdg_cache_home_is_honoured_when_set(self) -> None:
        with (
            mock.patch.dict(
                os.environ, {"XDG_CACHE_HOME": "/tmp/xdg-cache-probe"}, clear=False
            ),
            mock.patch.object(Path, "mkdir", return_value=None),
        ):
            path = Path(common.picker_cache_path("windows"))
        self.assertEqual(
            path, Path("/tmp/xdg-cache-probe/fuzzel/pickers/windows.cache")
        )


if __name__ == "__main__":
    unittest.main()
