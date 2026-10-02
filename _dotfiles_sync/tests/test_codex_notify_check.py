"""Behavior tests for check_codex_notify's Codex Computer Use case.

The silent failure: the check accepting a Computer Use notify line after the
app is gone. Codex's notify output goes nowhere, so nothing else would say so.
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from _dotfiles_sync import integration_checks


class CheckCodexNotifyTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.target = Path(self.tmp.name)
        (self.target / ".codex").mkdir()
        self.client = self.target / "Codex Computer Use.app" / "SkyComputerUseClient"

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def check(self) -> bool:
        (self.target / ".codex" / "config.toml").write_text(
            f'notify = [{json.dumps(str(self.client))}, "turn-ended"]\n'
        )
        return integration_checks.check_codex_notify(
            self.target, verbose=False, ignore=set()
        )

    def test_installed_computer_use_client_is_ok(self) -> None:
        self.client.parent.mkdir()
        self.client.touch()
        self.assertFalse(self.check())

    def test_missing_computer_use_client_warns(self) -> None:
        with self.assertLogs(integration_checks.LOGGER) as logs:
            self.assertTrue(self.check())
        self.assertIn("MISSING", "\n".join(logs.output))


if __name__ == "__main__":
    unittest.main()
