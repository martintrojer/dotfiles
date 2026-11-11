#!/usr/bin/env python3
"""Behavior tests for the vtmux → ~/.local/bin/tmux drop-in link.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

The silent failure is a stale link after `mise upgrade vtmux`: the path under
installs/ moves, plugins still call `tmux`, and nothing notices until a
`tmux -V` surprise. These tests pin missing, wrong, and correct states.
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import integration_checks


class VtmuxTmuxLinkTestCase(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.target = Path(tmp.name)
        self.want = self.target / "mise" / "installs" / "vtmux" / "bin" / "vtmux"
        self.want.parent.mkdir(parents=True)
        self.want.write_text("fake\n")
        self.link = self.target / ".local" / "bin" / "tmux"

    def run_check(self, *, vtmux: Path | None) -> tuple[bool, str]:
        with (
            mock.patch.object(integration_checks, "_vtmux_bin", return_value=vtmux),
            self.assertLogs(integration_checks.LOGGER, level="DEBUG") as logs,
        ):
            integration_checks.LOGGER.debug("sentinel")
            found = integration_checks.check_vtmux_tmux_link(
                self.target, verbose=True, ignore=set()
            )
        return found, "\n".join(logs.output)

    def test_missing_vtmux_reports_issue(self) -> None:
        found, output = self.run_check(vtmux=None)
        self.assertTrue(found)
        self.assertIn("MISSING", output)

    def test_missing_link_reports_drift(self) -> None:
        found, output = self.run_check(vtmux=self.want)
        self.assertTrue(found)
        self.assertIn("DRIFT", output)
        self.assertIn("missing", output)

    def test_wrong_link_reports_drift(self) -> None:
        self.link.parent.mkdir(parents=True)
        self.link.symlink_to(self.target / "old" / "vtmux")
        found, output = self.run_check(vtmux=self.want)
        self.assertTrue(found)
        self.assertIn("DRIFT", output)

    def test_correct_link_is_quiet(self) -> None:
        self.link.parent.mkdir(parents=True)
        self.link.symlink_to(self.want)
        found, output = self.run_check(vtmux=self.want)
        self.assertFalse(found)
        self.assertIn("OK:", output)

    def test_ignore_skips_check(self) -> None:
        with mock.patch.object(integration_checks, "_vtmux_bin", return_value=None):
            found = integration_checks.check_vtmux_tmux_link(
                self.target, verbose=False, ignore={"vtmux-tmux"}
            )
        self.assertFalse(found)

    def test_apply_creates_link(self) -> None:
        with mock.patch.object(
            integration_checks, "_vtmux_bin", return_value=self.want
        ):
            integration_checks.apply_vtmux_tmux_link(self.target, verbose=False)
        self.assertTrue(self.link.is_symlink())
        self.assertEqual(self.link.readlink(), self.want)

    def test_apply_rewrites_stale_link(self) -> None:
        self.link.parent.mkdir(parents=True)
        self.link.symlink_to(self.target / "old" / "vtmux")
        with mock.patch.object(
            integration_checks, "_vtmux_bin", return_value=self.want
        ):
            integration_checks.apply_vtmux_tmux_link(self.target, verbose=False)
        self.assertEqual(self.link.readlink(), self.want)

    def test_apply_skips_when_vtmux_missing(self) -> None:
        with (
            mock.patch.object(integration_checks, "_vtmux_bin", return_value=None),
            self.assertLogs(integration_checks.LOGGER, level="WARNING") as logs,
        ):
            integration_checks.apply_vtmux_tmux_link(self.target, verbose=False)
        self.assertFalse(self.link.exists())
        self.assertIn("SKIP", "\n".join(logs.output))


if __name__ == "__main__":
    unittest.main()
