#!/usr/bin/env python3
"""Behavior tests for check_murmur's two-PATH resolution.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

The check exists to catch a silent failure, so it can itself fail silently:
it used to ask only `shutil.which("murmur")`, which answers for *this*
process. The real callers are tmux hooks, and a tmux server keeps the PATH it
was started with, so a fresh `npm i -g` resolves in the terminal while the
status segment, the picker and the focus hooks still see nothing -- and the
check reported OK. These tests pin the four PATH combinations.
"""

from __future__ import annotations

import os
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import integration_checks


class MurmurCheckTestCase(unittest.TestCase):
    """A target root that already satisfies every non-PATH requirement.

    Identity and the pi extension are present so any warning the check emits
    is about PATH and nothing else.
    """

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.target = Path(tmp.name)

        state = self.target / ".local" / "state" / "murmur"
        state.mkdir(parents=True)
        (state / "identity.json").write_text("{}\n")

        extensions = self.target / ".pi" / "agent" / "extensions"
        extensions.mkdir(parents=True)
        (extensions / "murmur.ts").write_text("// stub\n")

        # MURMUR_STATE_DIR/XDG_STATE_HOME would redirect the identity lookup
        # away from the fixture, so a developer's own env could pass or fail
        # these tests for the wrong reason.
        env = mock.patch.dict(os.environ, {}, clear=False)
        env.start()
        self.addCleanup(env.stop)
        os.environ.pop("MURMUR_STATE_DIR", None)
        os.environ.pop("XDG_STATE_HOME", None)

    def run_check(
        self, *, on_own_path: bool, tmux_path: str | None, on_tmux_path: bool = False
    ) -> tuple[bool, list[str]]:
        def fake_which(cmd: str, path: str | None = None) -> str | None:
            if cmd != "murmur":
                return "/usr/bin/" + cmd
            if path is None:
                return "/opt/murmur/bin/murmur" if on_own_path else None
            return "/opt/murmur/bin/murmur" if on_tmux_path else None

        with (
            mock.patch.object(integration_checks.shutil, "which", fake_which),
            mock.patch.object(
                integration_checks, "_tmux_server_path", return_value=tmux_path
            ),
            self.assertLogs(integration_checks.LOGGER, level="DEBUG") as logs,
        ):
            integration_checks.LOGGER.debug("sentinel")
            found = integration_checks.check_murmur(
                self.target, verbose=False, ignore=set()
            )
        return found, logs.output

    def test_reachable_from_tmux_is_clean(self) -> None:
        found, output = self.run_check(
            on_own_path=True, tmux_path="/opt/murmur/bin", on_tmux_path=True
        )
        self.assertFalse(found)
        self.assertNotIn("UNREACHABLE", "\n".join(output))

    def test_on_own_path_but_not_tmux_is_unreachable(self) -> None:
        """The regression: installed and usable here, dead in every tmux hook."""
        found, output = self.run_check(
            on_own_path=True, tmux_path="/usr/bin:/bin", on_tmux_path=False
        )
        self.assertTrue(found)
        self.assertIn("UNREACHABLE", "\n".join(output))

    def test_absent_everywhere_is_missing(self) -> None:
        found, output = self.run_check(
            on_own_path=False, tmux_path="/usr/bin:/bin", on_tmux_path=False
        )
        self.assertTrue(found)
        joined = "\n".join(output)
        self.assertIn("MISSING", joined)
        self.assertNotIn("UNREACHABLE", joined)

    def test_no_tmux_server_falls_back_to_own_path(self) -> None:
        """No server to ask means no verdict to give -- do not invent one."""
        found, output = self.run_check(on_own_path=True, tmux_path=None)
        self.assertFalse(found)
        self.assertNotIn("UNREACHABLE", "\n".join(output))

        found, output = self.run_check(on_own_path=False, tmux_path=None)
        self.assertTrue(found)
        self.assertIn("MISSING", "\n".join(output))

    def test_tmux_only_install_is_not_reported_missing(self) -> None:
        """Reachable where it counts, even if this process cannot see it."""
        found, output = self.run_check(
            on_own_path=False, tmux_path="/opt/murmur/bin", on_tmux_path=True
        )
        self.assertFalse(found)
        self.assertNotIn("MISSING", "\n".join(output))


if __name__ == "__main__":
    unittest.main()
