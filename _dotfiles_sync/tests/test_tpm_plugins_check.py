#!/usr/bin/env python3
"""Behavior tests for check_tpm_plugins.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

A missing TPM plugin fails silently: tmux loads, and the plugin's keys and
options just never appear. That is what the check catches, so these tests pin
missing, stale, and clean against a fixture .tmux.conf.
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import integration_checks

CONF = """\
set -g @plugin 'tmux-plugins/tpm'
set -g @plugin "mu-crew/tmux-session-picker"
  set-option -g @plugin 'sainnhe/tmux-fzf'
# set -g @plugin 'commented/out'
set -g @fingers-key Tab
"""


class TpmPluginsCheckTest(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.target = Path(tmp.name)
        self.conf = self.target / "tmux.conf"
        self.conf.write_text(CONF)
        self.plugins = self.target / ".tmux" / "plugins"

    def install(self, *names: str) -> None:
        for name in names:
            (self.plugins / name).mkdir(parents=True)

    def run_check(self, ignore: set[str] | None = None) -> tuple[bool, list[str]]:
        with self.assertLogs("dotfiles-sync", level="DEBUG") as logs:
            # assertLogs fails on silence, so log a marker that tests can skip.
            integration_checks.LOGGER.debug("marker")
            found = integration_checks.check_tpm_plugins(
                self.target, verbose=False, ignore=ignore or set(), conf=self.conf
            )
        # lazy_header logs the "[tmux-plugins]" section header at WARNING too.
        warnings = [
            r.getMessage()
            for r in logs.records
            if r.levelname == "WARNING" and not r.getMessage().startswith("\n[")
        ]
        return found, warnings

    def test_parses_plugin_lines_only(self) -> None:
        self.assertEqual(
            integration_checks.tpm_plugins(self.conf),
            ["tmux-plugins/tpm", "mu-crew/tmux-session-picker", "sainnhe/tmux-fzf"],
        )

    def test_all_installed_is_clean(self) -> None:
        self.install("tpm", "tmux-session-picker", "tmux-fzf")
        self.assertEqual(self.run_check(), (False, []))

    def test_missing_plugin_is_reported(self) -> None:
        self.install("tpm", "tmux-fzf")
        found, warnings = self.run_check()
        self.assertTrue(found)
        self.assertEqual(len(warnings), 1)
        self.assertIn("MISSING: mu-crew/tmux-session-picker", warnings[0])

    def test_stale_plugin_dir_is_reported(self) -> None:
        self.install("tpm", "tmux-session-picker", "tmux-fzf", "tmux-cpu")
        found, warnings = self.run_check()
        self.assertTrue(found)
        self.assertEqual(len(warnings), 1)
        self.assertIn("STALE:", warnings[0])
        self.assertIn("tmux-cpu", warnings[0])

    def test_ignore_silences_both_kinds(self) -> None:
        self.install("tpm", "tmux-fzf", "tmux-cpu")
        ignore = {"tmux-plugin:tmux-session-picker", "tmux-plugin:tmux-cpu"}
        self.assertEqual(self.run_check(ignore), (False, []))

    def test_no_plugins_dir_reports_every_plugin_missing(self) -> None:
        found, warnings = self.run_check()
        self.assertTrue(found)
        self.assertEqual(len(warnings), 3)


if __name__ == "__main__":
    unittest.main()
