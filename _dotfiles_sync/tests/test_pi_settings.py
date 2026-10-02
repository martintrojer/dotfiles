"""Behavior tests for the pi settings merge.

The silent failure: --apply rewriting settings.json and dropping keys pi owns
(model, thinking level, changelog version). Nothing would say so; pi would
just start on a different model.
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from _dotfiles_sync import pi_settings


class PiSettingsTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.target = Path(self.tmp.name)
        self.path = self.target / pi_settings.SETTINGS_PATH

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def test_apply_merges_and_keeps_pi_owned_keys(self) -> None:
        self.path.parent.mkdir(parents=True)
        self.path.write_text(
            json.dumps(
                {
                    "defaultModel": "m",
                    "defaultTools": ["-bash"],
                    "terminal": {"showImages": False},
                }
            )
        )
        with self.assertLogs(pi_settings.LOGGER):
            self.assertTrue(
                pi_settings.check_pi_settings(self.target, verbose=False, ignore=set())
            )
        with self.assertLogs(pi_settings.LOGGER):
            pi_settings.apply_pi_settings(self.target, verbose=False)
        data = json.loads(self.path.read_text())
        self.assertEqual(data["defaultModel"], "m")
        self.assertEqual(data["defaultTools"], pi_settings.PI_SETTINGS["defaultTools"])
        self.assertEqual(
            data["terminal"], {"showImages": False, "showTerminalProgress": True}
        )
        self.assertFalse(
            pi_settings.check_pi_settings(self.target, verbose=False, ignore=set())
        )

    def test_apply_leaves_unparseable_file_alone(self) -> None:
        self.path.parent.mkdir(parents=True)
        self.path.write_text("{not json")
        with self.assertLogs(pi_settings.LOGGER):
            pi_settings.apply_pi_settings(self.target, verbose=False)
        self.assertEqual(self.path.read_text(), "{not json")


if __name__ == "__main__":
    unittest.main()
