"""Behavior tests for merging osc7501-hook entries into ~/.cursor/hooks.json.

The silent failure: --apply rewriting hooks.json and dropping hooks owned by
murmur, mu, muzzle, or museum, or duplicating entries on repeat runs.
"""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path

from _dotfiles_sync import osc7501_hooks


class Osc7501HooksTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.target = Path(self.tmp.name)
        self.cursor_path = self.target / osc7501_hooks.CURSOR_HOOKS_PATH

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def test_apply_merges_without_dropping_existing_hooks(self) -> None:
        self.cursor_path.parent.mkdir(parents=True)
        self.cursor_path.write_text(
            json.dumps(
                {
                    "version": 1,
                    "hooks": {
                        "stop": [{"command": "murmur hook cursor-agent"}],
                        "afterAgentResponse": [{"command": "murmur hook cursor-agent"}],
                    },
                }
            )
        )

        with self.assertLogs(osc7501_hooks.LOGGER):
            self.assertTrue(
                osc7501_hooks.check_osc7501_hooks(
                    self.target, verbose=False, ignore=set()
                )
            )
        with self.assertLogs(osc7501_hooks.LOGGER):
            osc7501_hooks.apply_osc7501_hooks(self.target, verbose=False)

        cursor = json.loads(self.cursor_path.read_text())
        self.assertEqual(cursor["version"], 1)
        self.assertEqual(
            cursor["hooks"]["stop"],
            [
                {"command": "murmur hook cursor-agent"},
                {"command": osc7501_hooks.CURSOR_COMMAND},
            ],
        )
        self.assertEqual(
            cursor["hooks"]["afterAgentResponse"],
            [{"command": "murmur hook cursor-agent"}],
        )

        # Idempotent: clean check and no duplicate entries on second apply.
        self.assertFalse(
            osc7501_hooks.check_osc7501_hooks(self.target, verbose=False, ignore=set())
        )
        osc7501_hooks.apply_osc7501_hooks(self.target, verbose=False)
        cursor_after = json.loads(self.cursor_path.read_text())
        self.assertEqual(len(cursor_after["hooks"]["stop"]), 2)

    def test_creates_missing_file_with_expected_shape(self) -> None:
        with self.assertLogs(osc7501_hooks.LOGGER):
            osc7501_hooks.apply_osc7501_hooks(self.target, verbose=False)
        cursor = json.loads(self.cursor_path.read_text())
        self.assertEqual(cursor["version"], 1)
        for event in osc7501_hooks.CURSOR_EVENTS:
            self.assertEqual(
                cursor["hooks"][event], [{"command": osc7501_hooks.CURSOR_COMMAND}]
            )
        self.assertFalse(
            osc7501_hooks.check_osc7501_hooks(self.target, verbose=False, ignore=set())
        )

    def test_leaves_unparseable_file_alone(self) -> None:
        self.cursor_path.parent.mkdir(parents=True)
        self.cursor_path.write_text("{not json")
        with self.assertLogs(osc7501_hooks.LOGGER):
            osc7501_hooks.apply_osc7501_hooks(self.target, verbose=False)
        self.assertEqual(self.cursor_path.read_text(), "{not json")

    def test_ignore_suppresses_check(self) -> None:
        self.assertFalse(
            osc7501_hooks.check_osc7501_hooks(
                self.target, verbose=False, ignore={osc7501_hooks.ISSUE_ID}
            )
        )


if __name__ == "__main__":
    unittest.main()
