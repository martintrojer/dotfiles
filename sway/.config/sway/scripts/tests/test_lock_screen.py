#!/usr/bin/env python3
"""Regression tests for lock-screen's silent failure modes.

A wrong "locker already running" answer skips the idle lock without any
error, and a missed fallback leaves the session unlocked. Neither is loud.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import tempfile
import unittest
from pathlib import Path
from typing import Any
from unittest import mock

SCRIPT = Path(__file__).parents[1] / "lock-screen"


def load() -> Any:
    loader = importlib.machinery.SourceFileLoader("lock_screen", str(SCRIPT))
    spec = importlib.util.spec_from_loader("lock_screen", loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def fake_proc(root: Path, *cmdlines: list[str]) -> Path:
    for pid, argv in enumerate(cmdlines, start=100):
        entry = root / str(pid)
        entry.mkdir()
        (entry / "cmdline").write_bytes(b"\0".join(a.encode() for a in argv) + b"\0")
    (root / "self").mkdir()
    return root


class LockerRunningTests(unittest.TestCase):
    def setUp(self) -> None:
        self.ls = load()

    def running(self, *cmdlines: list[str]) -> bool:
        with tempfile.TemporaryDirectory() as tmp:
            return self.ls.locker_running(fake_proc(Path(tmp), *cmdlines))

    def test_preview_window_does_not_count_as_a_lock(self) -> None:
        self.assertFalse(self.running(["/usr/bin/swaywardlock", "--preview"]))
        self.assertFalse(self.running(["swaywardlock", "validate"]))

    def test_real_lockers_count(self) -> None:
        self.assertTrue(self.running(["/home/u/.local/bin/swaywardlock", "-f"]))
        self.assertTrue(self.running(["swaylock", "-f", "--color", "1e1e2e"]))

    def test_unrelated_names_do_not_count(self) -> None:
        self.assertFalse(self.running(["vim", "swaywardlock"]))


class FallbackTests(unittest.TestCase):
    def setUp(self) -> None:
        self.ls = load()
        self.calls: list[list[str]] = []

    def main(self, results: dict[str, int], present: set[str]) -> int:
        def run(cmd: list[str]) -> int:
            self.calls.append(cmd)
            return results[cmd[0]]

        with (
            mock.patch.object(self.ls, "locker_running", return_value=False),
            mock.patch.object(self.ls, "run", side_effect=run),
            mock.patch.object(
                self.ls.shutil,
                "which",
                side_effect=lambda n: n if n in present else None,
            ),
            mock.patch("sys.stderr"),
        ):
            return self.ls.main(["--daemonize"])

    def test_swaywardlock_success_stops_there(self) -> None:
        rc = self.main({"swaywardlock": 0}, {"swaywardlock", "swaylock"})
        self.assertEqual(rc, 0)
        self.assertEqual(self.calls, [["swaywardlock", "-f"]])

    def test_failed_swaywardlock_falls_back_to_swaylock(self) -> None:
        rc = self.main({"swaywardlock": 1, "swaylock": 0}, {"swaywardlock", "swaylock"})
        self.assertEqual(rc, 0)
        self.assertEqual([c[0] for c in self.calls], ["swaywardlock", "swaylock"])
        self.assertIn("-f", self.calls[1])

    def test_missing_swaywardlock_falls_back_to_swaylock(self) -> None:
        rc = self.main({"swaylock": 0}, {"swaylock"})
        self.assertEqual(rc, 0)
        self.assertEqual([c[0] for c in self.calls], ["swaylock"])


if __name__ == "__main__":
    unittest.main()
