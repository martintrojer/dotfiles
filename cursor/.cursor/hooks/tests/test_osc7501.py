#!/usr/bin/env python3
"""Tests for cursor/.cursor/hooks/osc7501 (temporary Cursor OSC 7501 bridge)."""

from __future__ import annotations

import base64
import importlib.machinery
import importlib.util
import io
import json
import subprocess
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = Path(__file__).parents[1] / "osc7501"


def load():
    loader = importlib.machinery.SourceFileLoader("cursor_osc7501", str(SCRIPT))
    spec = importlib.util.spec_from_loader("cursor_osc7501", loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def b64(text: str) -> str:
    return base64.b64encode(text.encode("utf-8")).decode("ascii")


class FormatTests(unittest.TestCase):
    def test_sanitize_strips_c0_del_c1_and_collapses_whitespace(self) -> None:
        mod = load()
        self.assertEqual(
            mod.sanitize_text("line1\n\tline2\x1b[31m\x7f\x85end"),
            "line1 line2 [31m end",
        )

    def test_clear_omits_other_fields(self) -> None:
        mod = load()
        self.assertEqual(
            mod.format_osc7501("clear", app="cursor", msg="ignored"),
            b"\x1b]7501;state=clear\x1b\\",
        )

    def test_kind_only_emitted_when_blocked(self) -> None:
        mod = load()
        self.assertEqual(
            mod.format_osc7501("working", app="cursor", kind="permission"),
            b"\x1b]7501;state=working:app=cursor\x1b\\",
        )
        self.assertEqual(
            mod.format_osc7501("blocked", app="cursor", kind="permission", msg="Bash"),
            f"\x1b]7501;state=blocked:app=cursor:kind=permission:msg={b64('Bash')}\x1b\\".encode(
                "ascii"
            ),
        )


class CursorMappingTests(unittest.TestCase):
    def test_lifecycle_events(self) -> None:
        mod = load()
        self.assertEqual(
            mod.map_cursor({"hook_event_name": "sessionStart"}),
            b"\x1b]7501;state=idle:app=cursor\x1b\\",
        )
        self.assertEqual(
            mod.map_cursor({"hook_event_name": "beforeSubmitPrompt"}),
            b"\x1b]7501;state=working:app=cursor\x1b\\",
        )
        self.assertEqual(
            mod.map_cursor(
                {"hook_event_name": "beforeShellExecution", "command": "pytest -q"}
            ),
            f"\x1b]7501;state=working:app=cursor:msg={b64('pytest')}\x1b\\".encode(
                "ascii"
            ),
        )
        self.assertIsNone(
            mod.map_cursor({"hook_event_name": "afterAgentResponse", "text": "hi"})
        )
        self.assertEqual(
            mod.map_cursor({"hook_event_name": "stop", "status": "completed"}),
            b"\x1b]7501;state=done:app=cursor\x1b\\",
        )
        self.assertEqual(
            mod.map_cursor({"hook_event_name": "stop", "status": "aborted"}),
            b"\x1b]7501;state=idle:app=cursor\x1b\\",
        )
        self.assertEqual(
            mod.map_cursor(
                {
                    "hook_event_name": "stop",
                    "status": "error",
                    "error_message": "rate limit",
                }
            ),
            f"\x1b]7501;state=error:app=cursor:msg={b64('rate limit')}\x1b\\".encode(
                "ascii"
            ),
        )
        self.assertEqual(
            mod.map_cursor({"hook_event_name": "sessionEnd"}),
            b"\x1b]7501;state=clear\x1b\\",
        )


class TtyAndMainTests(unittest.TestCase):
    def test_ancestor_tty_walks_past_detached_parents(self) -> None:
        mod = load()
        responses = [
            subprocess.CompletedProcess([], 0, stdout="  200 ??\n", stderr=""),
            subprocess.CompletedProcess([], 0, stdout="  100 ttys004\n", stderr=""),
        ]
        with mock.patch.object(subprocess, "run", side_effect=responses):
            self.assertEqual(mod._ancestor_tty(300), "/dev/ttys004")

    def test_main_exits_zero_on_bad_json(self) -> None:
        mod = load()
        with mock.patch("sys.stdin", io.StringIO("{not json")):
            self.assertEqual(mod.main(), 0)

    def test_main_writes_mapped_sequence(self) -> None:
        mod = load()
        payload = json.dumps({"hook_event_name": "beforeSubmitPrompt"})
        with (
            mock.patch("sys.stdin", io.StringIO(payload)),
            mock.patch.object(mod, "write_to_tty", return_value=True) as writer,
        ):
            self.assertEqual(mod.main(), 0)
            writer.assert_called_once_with(b"\x1b]7501;state=working:app=cursor\x1b\\")


if __name__ == "__main__":
    unittest.main()
