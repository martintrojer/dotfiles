#!/usr/bin/env python3
"""Regression tests for the desktop session logout path."""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).parents[1] / "session-quit"


class SessionQuitTests(unittest.TestCase):
    def test_queues_cleanup_before_exiting_without_waiting(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            calls = root / "calls"
            for command in ("systemctl", "swaymsg"):
                executable = root / command
                executable.write_text(
                    '#!/bin/sh\nprintf "%s\\n" "$(basename "$0") $*" >> "$CALLS"\n'
                )
                executable.chmod(0o755)

            env = os.environ | {
                "CALLS": str(calls),
                "PATH": f"{root}:{os.environ['PATH']}",
            }
            result = subprocess.run([SCRIPT], env=env, check=False)

            self.assertEqual(result.returncode, 0)
            self.assertEqual(
                calls.read_text().splitlines(),
                [
                    "systemctl --user --no-block stop sway-session.target",
                    "swaymsg exit",
                ],
            )


if __name__ == "__main__":
    unittest.main()
