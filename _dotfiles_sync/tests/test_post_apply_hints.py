"""Behavior tests for print_post_apply_hints."""

from __future__ import annotations

import io
import unittest
from contextlib import redirect_stdout

from _dotfiles_sync.cli import print_post_apply_hints


class PostApplyHintsTest(unittest.TestCase):
    def hints(self) -> str:
        buf = io.StringIO()
        with redirect_stdout(buf):
            print_post_apply_hints()
        return buf.getvalue()

    def test_prints_codex_murmur_notify_line(self) -> None:
        text = self.hints()
        self.assertIn("~/.codex/config.toml", text)
        self.assertIn('notify = ["murmur", "notify", "--source", "codex"]', text)
        # A bare `sh -lc '<script>'` wrapper puts the event JSON Codex appends
        # into $0, so murmur never sees which event fired (docs/SETUP.md § 6).
        self.assertNotIn('"-lc"', text)

    def test_prints_cursor_murmur_stop_hook(self) -> None:
        text = self.hints()
        self.assertIn("~/.cursor/hooks.json", text)
        self.assertIn("murmur notify --source cursor", text)


if __name__ == "__main__":
    unittest.main()
