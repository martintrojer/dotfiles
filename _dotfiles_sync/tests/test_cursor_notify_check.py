"""Behavior tests for check_cursor_notify."""

from __future__ import annotations

import json
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from _dotfiles_sync import integration_checks


class CheckCursorNotifyTest(unittest.TestCase):
    def setUp(self) -> None:
        self.tmp = tempfile.TemporaryDirectory()
        self.target = Path(self.tmp.name)
        (self.target / ".cursor").mkdir()

    def tearDown(self) -> None:
        self.tmp.cleanup()

    def write_hooks(self, data: object) -> Path:
        path = self.target / ".cursor" / "hooks.json"
        path.write_text(json.dumps(data))
        return path

    def run_check(self, *, verbose: bool = False) -> tuple[bool, str]:
        with self.assertLogs(integration_checks.LOGGER, level="DEBUG") as logs:
            integration_checks.LOGGER.debug("sentinel")
            found = integration_checks.check_cursor_notify(
                self.target, verbose=verbose, ignore=set()
            )
        return found, "\n".join(logs.output)

    def test_missing_hooks_file_is_clean(self) -> None:
        found, _ = self.run_check()
        self.assertFalse(found)

    def test_stop_hook_calling_murmur_is_ok(self) -> None:
        self.write_hooks(
            {
                "version": 1,
                "hooks": {
                    "stop": [{"command": "murmur notify --source cursor"}],
                },
            }
        )
        found, output = self.run_check(verbose=True)
        self.assertFalse(found)
        self.assertIn("OK: Cursor stop hook calls murmur", output)

    def test_stop_hook_without_murmur_warns(self) -> None:
        self.write_hooks(
            {
                "version": 1,
                "hooks": {"stop": [{"command": "./hooks/something-else.sh"}]},
            }
        )
        found, output = self.run_check()
        self.assertTrue(found)
        self.assertIn("UNKNOWN: Cursor stop hooks", output)

    def test_invalid_json_warns(self) -> None:
        path = self.target / ".cursor" / "hooks.json"
        path.write_text("{not-json")
        found, output = self.run_check()
        self.assertTrue(found)
        self.assertIn("INVALID: Cursor hooks", output)

    def test_ignore_suppresses_warning(self) -> None:
        self.write_hooks(
            {
                "version": 1,
                "hooks": {"stop": [{"command": "./hooks/something-else.sh"}]},
            }
        )
        with mock.patch.object(integration_checks, "lazy_header") as header:
            found = integration_checks.check_cursor_notify(
                self.target, verbose=False, ignore={"cursor-notify"}
            )
        self.assertFalse(found)
        header.assert_not_called()


if __name__ == "__main__":
    unittest.main()
