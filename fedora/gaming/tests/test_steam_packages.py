#!/usr/bin/env python3
"""Behavior tests for gaming package setup and Python helpers."""

from __future__ import annotations

import signal
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Any, cast
from unittest import mock

sys.path.insert(0, str(Path(__file__).parents[2] / "tests"))

# Imported after the sys.path line above.
import _package_harness as harness

FEDORA = Path(__file__).parents[2]
STEAM_PAUSE = FEDORA / "gaming/home/.local/bin/steam-pause"
load_script = harness.load_script

# (package array script, wrapper script, install command the wrapper uses)
GAMING_SETUPS = [
    (
        FEDORA / "gaming/os/steam-packages.sh",
        FEDORA / "gaming/os/setup-steam.sh",
        "rpm-ostree install",
    ),
]


class PackageArrays(harness.PackageArrayChecks):
    SETUPS = GAMING_SETUPS


class GamingPythonHelperTests(unittest.TestCase):
    def test_steam_pause_parses_proc_tree_and_signals_only_descendants(self) -> None:
        steam_pause = cast(Any, load_script("steam_pause_test", STEAM_PAUSE))
        with tempfile.TemporaryDirectory() as raw_tmp:
            proc = Path(raw_tmp)
            processes = {
                100: (1, "reaper", b"/usr/bin/reaper\0SteamLaunch\0AppId=42\0--\0"),
                101: (100, "game worker", b"game\0"),
                102: (101, "child ) worker", b"child\0"),
                200: (1, "unrelated", b"other\0"),
            }
            for pid, (ppid, name, cmdline) in processes.items():
                pid_dir = proc / str(pid)
                pid_dir.mkdir()
                (pid_dir / "cmdline").write_bytes(cmdline)
                (pid_dir / "stat").write_text(f"{pid} ({name}) S {ppid} 0 0 0\n")
            steam_pause.PROC = proc

            self.assertEqual(steam_pause.game_reapers(), [(100, 42)])
            kids = steam_pause.children_map()
            self.assertEqual(steam_pause.descendants(100, kids), [101, 102])
            with mock.patch.object(steam_pause.os, "kill") as kill:
                count = steam_pause.signal_tree(100, signal.SIGSTOP, kids)

            self.assertEqual(count, 2)
            self.assertEqual(
                kill.call_args_list,
                [mock.call(101, signal.SIGSTOP), mock.call(102, signal.SIGSTOP)],
            )

    def test_steam_pause_reports_paused_only_when_whole_tree_is_stopped(self) -> None:
        steam_pause = cast(Any, load_script("steam_pause_test", STEAM_PAUSE))
        with tempfile.TemporaryDirectory() as raw_tmp:
            proc = Path(raw_tmp)
            steam_pause.PROC = proc

            def set_states(states: dict[int, str]) -> None:
                for pid, state in states.items():
                    pid_dir = proc / str(pid)
                    pid_dir.mkdir(exist_ok=True)
                    (pid_dir / "stat").write_text(f"{pid} (game ) x) {state} 100 0 0\n")

            set_states({101: "R", 102: "T"})
            # Order must not matter: descendants() pops off a stack, so which
            # pid comes first is arbitrary between runs.
            self.assertFalse(steam_pause.is_paused([101, 102]))
            self.assertFalse(steam_pause.is_paused([102, 101]))

            set_states({101: "T", 102: "T"})
            self.assertTrue(steam_pause.is_paused([101, 102]))

            # Vanished pids are ignored, but an entirely gone tree is not paused.
            self.assertTrue(steam_pause.is_paused([101, 102, 999]))
            self.assertFalse(steam_pause.is_paused([999]))
            self.assertFalse(steam_pause.is_paused([]))


if __name__ == "__main__":
    unittest.main()
