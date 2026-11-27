"""Tests for the OSC 7577 half of cursor/.cursor/hooks/osc7501.

The silent failure: vtmux ignores a report with a bad MAC, stale time or
malformed body without any error, so the pane just loses its resume record.
`verify` mirrors vtmux's D35 verifier (program_resume.rs, rev 0.5).
"""

from __future__ import annotations

import base64
import hashlib
import importlib.machinery
import importlib.util
import io
import json
import os
import re
import unittest
from pathlib import Path
from unittest import mock

HOOK = Path(__file__).resolve().parents[2] / "cursor/.cursor/hooks/osc7501"
TOKEN = "AbC_def-0123456789abcdefghijklmn"
NOW = 1795543200


def _load():
    loader = importlib.machinery.SourceFileLoader("osc7501_hook", str(HOOK))
    spec = importlib.util.spec_from_loader("osc7501_hook", loader)
    assert spec is not None
    mod = importlib.util.module_from_spec(spec)
    loader.exec_module(mod)
    return mod


hook = _load()


def _text(v: str) -> str | None:
    try:
        s = base64.b64decode(v + "=" * (-len(v) % 4), validate=True).decode()
    except ValueError:
        return None
    return None if re.search(r"[\x00-\x1f\x7f-\x9f]", s) else s


def verify(seq: bytes, token: str, now: int) -> dict | None:
    m = re.fullmatch(rb"\x1b\]7577;(.*)\x1b\\", seq, re.S)
    if not m:
        return None
    body = m.group(1).decode()
    if len(body) > 4096 or ":mac=" not in body:
        return None
    prefix, _, mac = body.rpartition(":mac=")
    if not re.fullmatch(r"[0-9a-f]{64}", mac):
        return None
    if hashlib.sha256(f"{prefix}:{token}".encode()).hexdigest() != mac:
        return None
    kv: dict[str, str] = {}
    for pair in prefix.split(":"):
        k, sep, v = pair.partition("=")
        if not sep or not re.fullmatch(r"[a-z]+", k) or k in kv or k == "mac":
            return None
        if not re.fullmatch(r"[A-Za-z0-9_.,+/=-]*", v):
            return None
        kv[k] = v
    uint = re.compile(r"[1-9][0-9]*")
    if not (uint.fullmatch(kv.get("pid", "")) and uint.fullmatch(kv.get("time", ""))):
        return None
    if abs(int(kv["time"]) - now) > 60:
        return None
    out: dict = {"pid": int(kv["pid"])}
    if "cwd" in kv:
        cwd = _text(kv["cwd"])
        if not cwd or not cwd.startswith("/"):
            return None
        out["cwd"] = cwd
    if ("argv" in kv) == (kv.get("clear") == "1"):
        return None
    if "argv" in kv:
        argv = [_text(e) for e in kv["argv"].split(",")]
        if len(argv) > 128 or any(a is None for a in argv):
            return None
        first = argv[0]
        if not first or not (
            first.startswith("/") or ("/" not in first and not first.startswith("-"))
        ):
            return None
        out["argv"] = argv
    else:
        out["clear"] = True
    return out


class ValidationTest(unittest.TestCase):
    def test_control_chars_rejected(self) -> None:
        for bad in ("a\x00b", "a\nb", "a\x1bb", "a\x7fb", "a\x85b"):
            self.assertIsNone(hook.format_osc7577(TOKEN, 1, NOW, argv=["x", bad]))
            self.assertIsNone(
                hook.format_osc7577(TOKEN, 1, NOW, argv=["x"], cwd="/" + bad)
            )

    def test_oversized_body_rejected(self) -> None:
        self.assertIsNone(hook.format_osc7577(TOKEN, 1, NOW, argv=["x", "a" * 4000]))
        seq = hook.format_osc7577(TOKEN, 1, NOW, argv=["x", "a" * 2000])
        self.assertIsNotNone(verify(seq, TOKEN, NOW))

    def test_body_cap_boundary(self) -> None:
        def size(n: int) -> int:
            seq = hook.format_osc7577(TOKEN, 1, NOW, argv=["x", "a" * n])
            return len(seq) - len(b"\x1b]7577;\x1b\\") if seq else 10**9

        lo = max(n for n in range(2900, 3100) if size(n) <= 4096)
        self.assertLessEqual(size(lo), 4096)
        self.assertIsNone(
            hook.format_osc7577(TOKEN, 1, NOW, argv=["x", "a" * (lo + 3)])
        )

    def test_relative_cwd_omitted(self) -> None:
        seq = hook.format_osc7577(TOKEN, 1, NOW, argv=["x"], cwd="rel/dir")
        self.assertEqual(verify(seq, TOKEN, NOW), {"pid": 1, "argv": ["x"]})


class OwnerPidTest(unittest.TestCase):
    def _run(self, table: dict[int, tuple[int, str]], start: int) -> int:
        def fake(pid: int, fields: str) -> list[str] | None:
            row = table.get(pid)
            return [str(row[0]), row[1]] if row else None

        with (
            mock.patch.object(hook, "_ps_row", fake),
            mock.patch.object(hook.os, "getppid", return_value=start),
        ):
            return hook.owner_pid()

    def test_skips_shell_wrappers(self) -> None:
        table = {
            500: (400, "/bin/sh"),
            400: (300, "-zsh"),
            300: (200, "/usr/bin/cursor-agent"),
        }
        self.assertEqual(self._run(table, 500), 300)

    def test_direct_parent_kept(self) -> None:
        self.assertEqual(self._run({500: (1, "cursor-agent")}, 500), 500)

    def test_all_shells_falls_back_to_getppid(self) -> None:
        table = {500: (400, "sh"), 400: (1, "bash")}
        self.assertEqual(self._run(table, 500), 500)


class WriteAllTest(unittest.TestCase):
    def test_short_writes_complete(self) -> None:
        chunks: list[bytes] = []

        def fake(fd: int, data: memoryview) -> int:
            b = bytes(data)[:3]
            chunks.append(b)
            return len(b)

        with mock.patch.object(hook.os, "write", fake):
            self.assertTrue(hook.write_all(9, b"abcdefgh", sleep=lambda _: None))
        self.assertEqual(b"".join(chunks), b"abcdefgh")

    def test_eagain_then_success(self) -> None:
        calls = iter([BlockingIOError(), 4, BlockingIOError(), 4])

        def fake(fd: int, data: object) -> int:
            v = next(calls)
            if isinstance(v, Exception):
                raise v
            return v

        with mock.patch.object(hook.os, "write", fake):
            self.assertTrue(hook.write_all(9, b"abcdefgh", sleep=lambda _: None))

    def test_persistent_eagain_gives_up(self) -> None:
        def fake(fd: int, data: object) -> int:
            raise BlockingIOError()

        with mock.patch.object(hook.os, "write", fake):
            self.assertFalse(hook.write_all(9, b"abc", retries=3, sleep=lambda _: None))

    def test_partial_then_eagain_is_failure(self) -> None:
        calls = iter([2])

        def fake(fd: int, data: object) -> int:
            try:
                return next(calls)
            except StopIteration:
                raise BlockingIOError() from None

        with mock.patch.object(hook.os, "write", fake):
            self.assertFalse(
                hook.write_all(9, b"abcdef", retries=2, sleep=lambda _: None)
            )


class FormatTest(unittest.TestCase):
    def test_argv_roundtrip(self) -> None:
        seq = hook.format_osc7577(
            TOKEN,
            4242,
            NOW,
            argv=["cursor-agent", "--resume", "5bb4f256-aaaa"],
            cwd="/tmp/my proj/ü",
        )
        self.assertEqual(
            verify(seq, TOKEN, NOW),
            {
                "pid": 4242,
                "argv": ["cursor-agent", "--resume", "5bb4f256-aaaa"],
                "cwd": "/tmp/my proj/ü",
            },
        )

    def test_clear(self) -> None:
        seq = hook.format_osc7577(TOKEN, 4242, NOW)
        self.assertEqual(verify(seq, TOKEN, NOW), {"pid": 4242, "clear": True})
        self.assertNotIn(b"argv=", seq)

    def test_known_mac_vector(self) -> None:
        seq = hook.format_osc7577("tok", 74877, NOW)
        mac = hashlib.sha256(b"pid=74877:time=1795543200:clear=1:tok").hexdigest()
        self.assertEqual(
            seq, f"\x1b]7577;pid=74877:time=1795543200:clear=1:mac={mac}\x1b\\".encode()
        )

    def test_verifier_rejects_tampering(self) -> None:
        seq = hook.format_osc7577(TOKEN, 7, NOW, argv=["cursor-agent"], cwd="/x")
        self.assertIsNone(verify(seq, "other", NOW))
        self.assertIsNone(verify(seq, TOKEN, NOW + 61))
        self.assertIsNone(verify(seq.replace(b"pid=7", b"pid=8"), TOKEN, NOW))


class MapTest(unittest.TestCase):
    def run_map(self, payload: dict) -> dict | None:
        seq = hook.map_cursor_resume(payload, TOKEN, 555, NOW)
        return None if seq is None else verify(seq, TOKEN, NOW)

    def test_session_start(self) -> None:
        r = self.run_map(
            {
                "hook_event_name": "sessionStart",
                "session_id": "sid-1",
                "workspace_roots": ["/work/repo"],
            }
        )
        self.assertEqual(
            r,
            {
                "pid": 555,
                "argv": ["cursor-agent", "--resume", "sid-1"],
                "cwd": "/work/repo",
            },
        )

    def test_conversation_id_preferred_on_submit(self) -> None:
        r = self.run_map(
            {
                "hook_event_name": "beforeSubmitPrompt",
                "conversation_id": "conv-9",
                "session_id": "other",
                "workspace_roots": ["/w"],
            }
        )
        assert r is not None
        self.assertEqual(r["argv"][2], "conv-9")

    def test_session_end_clears(self) -> None:
        self.assertEqual(
            self.run_map({"hook_event_name": "sessionEnd", "session_id": "s"}),
            {"pid": 555, "clear": True},
        )

    def test_no_id_or_other_event_emits_nothing(self) -> None:
        self.assertIsNone(self.run_map({"hook_event_name": "sessionStart"}))
        self.assertIsNone(
            self.run_map({"hook_event_name": "sessionStart", "session_id": " "})
        )
        for event in ("stop", "preToolUse", "postToolUse"):
            self.assertIsNone(
                self.run_map({"hook_event_name": event, "conversation_id": "c"})
            )

    def test_relative_workspace_root_falls_back_to_cwd(self) -> None:
        r = self.run_map(
            {
                "hook_event_name": "sessionStart",
                "session_id": "s",
                "workspace_roots": ["rel"],
            }
        )
        assert r is not None
        self.assertEqual(r["cwd"], os.getcwd())


class MainTest(unittest.TestCase):
    def run_main(self, payload: dict, env: dict[str, str]) -> bytes | None:
        written: list[bytes] = []
        with (
            mock.patch.dict(os.environ, env, clear=False),
            mock.patch("sys.stdin", io.StringIO(json.dumps(payload))),
            mock.patch.object(hook, "write_to_tty", lambda b: written.append(b)),
            mock.patch.object(hook.time, "time", lambda: NOW),
            mock.patch.object(hook, "owner_pid", lambda: os.getppid()),
        ):
            if "OSC7577_TOKEN" not in env:
                os.environ.pop("OSC7577_TOKEN", None)
            self.assertEqual(hook.main(), 0)
        return b"".join(written) if written else None

    def test_no_token_means_only_7501(self) -> None:
        out = self.run_main({"hook_event_name": "sessionStart", "session_id": "s"}, {})
        assert out is not None
        self.assertIn(b"\x1b]7501;", out)
        self.assertNotIn(b"7577", out)

    def test_token_adds_verifiable_7577_after_7501(self) -> None:
        out = self.run_main(
            {
                "hook_event_name": "sessionStart",
                "session_id": "s",
                "workspace_roots": ["/w"],
            },
            {"OSC7577_TOKEN": TOKEN},
        )
        assert out is not None
        head, _, tail = out.partition(b"\x1b]7577;")
        self.assertTrue(head.startswith(b"\x1b]7501;"))
        r = verify(b"\x1b]7577;" + tail, TOKEN, NOW)
        assert r is not None
        self.assertEqual(r["pid"], os.getppid())
        self.assertEqual(r["argv"], ["cursor-agent", "--resume", "s"])

    def test_session_end_emits_clear_for_both(self) -> None:
        out = self.run_main(
            {"hook_event_name": "sessionEnd", "session_id": "s"},
            {"OSC7577_TOKEN": TOKEN},
        )
        assert out is not None
        self.assertIn(b"state=clear", out)
        r = verify(b"\x1b]7577;" + out.partition(b"\x1b]7577;")[2], TOKEN, NOW)
        self.assertEqual(r, {"pid": os.getppid(), "clear": True})

    def test_both_sequences_for_submit(self) -> None:
        # beforeSubmitPrompt maps to 7501 working plus exactly one 7577 record.
        out = self.run_main(
            {"hook_event_name": "beforeSubmitPrompt", "conversation_id": "c"},
            {"OSC7577_TOKEN": TOKEN},
        )
        assert out is not None
        self.assertEqual(out.count(b"\x1b]7577;"), 1)


if __name__ == "__main__":
    unittest.main()
