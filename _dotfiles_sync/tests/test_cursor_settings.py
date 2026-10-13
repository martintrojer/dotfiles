"""Behavior tests for the Cursor CLI settings merge.

The silent failure: --apply rewriting cli-config.json and dropping keys the
CLI owns (auth, model, caches). Nothing would say so; the next agent launch
would start logged out or on a different model.
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from _dotfiles_sync import cursor_settings
from _dotfiles_sync.json_keys import LOGGER


class CursorSettingsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.target = Path(self.tmp.name)
        self.path = self.target / cursor_settings.SETTINGS_PATH

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def test_apply_merges_and_keeps_cli_owned_keys(self) -> None:
        self.path.parent.mkdir(parents=True)
        self.path.write_text(
            json.dumps(
                {
                    "approvalMode": "allowlist",
                    "authInfo": {"email": "kept@example.com"},
                    "permissions": {"allow": ["Shell(ls)"], "deny": []},
                }
            )
        )
        with self.assertLogs(LOGGER):
            self.assertTrue(
                cursor_settings.check_cursor_settings(
                    self.target, verbose=False, ignore=set()
                )
            )
        with self.assertLogs(LOGGER):
            cursor_settings.apply_cursor_settings(self.target, verbose=False)
        data = json.loads(self.path.read_text())
        self.assertEqual(data["authInfo"], {"email": "kept@example.com"})
        self.assertEqual(data["permissions"], {"allow": ["Shell(ls)"], "deny": []})
        self.assertEqual(
            data["approvalMode"], cursor_settings.CURSOR_SETTINGS["approvalMode"]
        )
        self.assertFalse(
            cursor_settings.check_cursor_settings(
                self.target, verbose=False, ignore=set()
            )
        )

    def test_apply_leaves_unparseable_file_alone(self) -> None:
        self.path.parent.mkdir(parents=True)
        self.path.write_text("{not json")
        with self.assertLogs(LOGGER):
            cursor_settings.apply_cursor_settings(self.target, verbose=False)
        self.assertEqual(self.path.read_text(), "{not json")


if __name__ == "__main__":
    unittest.main()
