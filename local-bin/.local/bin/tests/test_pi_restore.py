#!/usr/bin/env python3
"""Tests for the pi-restore picker: filter, prune, title fallback, restore."""

from __future__ import annotations

import contextlib
import importlib.machinery
import importlib.util
import io
import json
import os
import tempfile
import unittest
from pathlib import Path
from unittest import mock

SCRIPT = Path(__file__).parents[1] / "pi-restore"


def load():
    loader = importlib.machinery.SourceFileLoader("pi_restore", str(SCRIPT))
    spec = importlib.util.spec_from_loader("pi_restore", loader)
    assert spec is not None
    mod = importlib.util.module_from_spec(spec)
    loader.exec_module(mod)
    return mod


pr = load()
NOW = 10_000_000_000_000
DAY = 24 * 3600 * 1000


def entry(state="lost", starred=False, pid=1, age_ms=0, **kw):
    e = {
        "file": "/x.jsonl",
        "cwd": "/tmp",
        "tmuxSession": "s",
        "pid": pid,
        "startedAt": 0,
        "lastActiveAt": NOW - age_ms,
        "starred": starred,
        "state": state,
    }
    e.update(kw)
    return e


def ids(state, pi_pids=()):
    with mock.patch.object(pr, "is_pi_process", lambda pid: pid in pi_pids):
        rows, prune = pr.visible(state, NOW)
    return [r[0] for r in rows], prune


class FilterTests(unittest.TestCase):
    def test_alive_pi_pid_hidden(self):
        self.assertEqual(ids({"a": entry("lost", pid=5)}, pi_pids={5}), ([], []))

    def test_alive_pi_pid_hidden_even_if_starred(self):
        self.assertEqual(
            ids({"a": entry("closed", starred=True, pid=5)}, pi_pids={5})[0], []
        )

    def test_dead_pid_shown(self):
        self.assertEqual(ids({"a": entry("lost", pid=5)})[0], ["a"])

    def test_alive_non_pi_pid_shown(self):
        # pid reused by another program: is_pi_process says False
        self.assertEqual(ids({"a": entry("lost", pid=5)}, pi_pids={9})[0], ["a"])

    def test_live_with_dead_pid_is_lost(self):
        self.assertEqual(ids({"a": entry("live", pid=5)})[0], ["a"])

    def test_live_with_alive_pi_hidden(self):
        self.assertEqual(ids({"a": entry("live", pid=5)}, pi_pids={5})[0], [])

    def test_closed_unstarred_hidden(self):
        self.assertEqual(ids({"a": entry("closed")})[0], [])

    def test_closed_starred_shown(self):
        self.assertEqual(ids({"a": entry("closed", starred=True)})[0], ["a"])

    def test_old_lost_pruned(self):
        self.assertEqual(ids({"a": entry("lost", age_ms=15 * DAY)}), ([], ["a"]))

    def test_recent_lost_kept(self):
        self.assertEqual(ids({"a": entry("lost", age_ms=13 * DAY)}), (["a"], []))

    def test_old_starred_never_pruned(self):
        self.assertEqual(
            ids({"a": entry("lost", starred=True, age_ms=99 * DAY)}), (["a"], [])
        )

    def test_sorted_newest_first(self):
        s = {"old": entry(age_ms=2 * DAY), "new": entry(age_ms=1000)}
        self.assertEqual(ids(s)[0], ["new", "old"])


def write_session(path, lines):
    Path(path).write_text("\n".join(json.dumps(x) for x in lines) + "\n")


def user(text):
    return {"type": "message", "message": {"role": "user", "content": text}}


def asst(text):
    return {
        "type": "message",
        "message": {"role": "assistant", "content": [{"type": "text", "text": text}]},
    }


class TitleTests(unittest.TestCase):
    def setUp(self):
        t = tempfile.TemporaryDirectory()
        self.addCleanup(t.cleanup)
        self.f = os.path.join(t.name, "s.jsonl")

    def test_last_session_info_wins(self):
        write_session(
            self.f,
            [
                {"type": "session"},
                user("first prompt"),
                {"type": "session_info", "name": "one"},
                {"type": "session_info", "name": "two"},
            ],
        )
        self.assertEqual(pr.title_of(self.f), "two")

    def test_falls_back_to_first_prompt_and_truncates(self):
        write_session(self.f, [user("x" * 200), user("second")])
        t = pr.title_of(self.f)
        self.assertTrue(t.startswith("xxx"))
        self.assertLessEqual(len(t), pr.TITLE_MAX)

    def test_block_content_prompt(self):
        m = {"role": "user", "content": [{"type": "text", "text": "hello"}]}
        write_session(self.f, [{"type": "message", "message": m}])
        self.assertEqual(pr.title_of(self.f), "hello")

    def test_missing_file_is_untitled(self):
        self.assertEqual(pr.title_of(self.f + ".nope"), "(untitled)")


class StateTests(unittest.TestCase):
    def setUp(self):
        t = tempfile.TemporaryDirectory()
        self.addCleanup(t.cleanup)
        self.p = Path(t.name) / "restore.json"
        p = mock.patch.dict(os.environ, {"PI_RESTORE_STATE": str(self.p)})
        p.start()
        self.addCleanup(p.stop)

    def test_missing_and_empty_are_no_entries(self):
        self.assertEqual(pr.load_state(), {})
        self.p.write_text("")
        self.assertEqual(pr.load_state(), {})

    def test_corrupt_not_overwritten(self):
        self.p.write_text("{nope")
        self.assertEqual(pr.load_state(), {})
        pr.cmd_star("a")
        pr.cmd_dismiss("a")
        self.assertEqual(self.p.read_text(), "{nope")

    def test_star_toggle_and_dismiss(self):
        self.p.write_text(json.dumps({"a": entry()}))
        pr.cmd_star("a")
        self.assertTrue(json.loads(self.p.read_text())["a"]["starred"])
        pr.cmd_star("a")
        self.assertFalse(json.loads(self.p.read_text())["a"]["starred"])
        pr.cmd_dismiss("a")
        self.assertEqual(json.loads(self.p.read_text()), {})


class PickPreviewTests(unittest.TestCase):
    def setUp(self):
        t = tempfile.TemporaryDirectory()
        self.addCleanup(t.cleanup)
        self.root = Path(t.name)
        self.p = self.root / "restore.json"
        env = mock.patch.dict(os.environ, {"PI_RESTORE_STATE": str(self.p)})
        env.start()
        self.addCleanup(env.stop)

    def test_empty_run_still_prunes(self):
        old = entry("lost", pid=999999, age_ms=15 * DAY)
        old["lastActiveAt"] = pr.now_ms() - 15 * DAY
        self.p.write_text(json.dumps({"a": old}))
        with (
            mock.patch.object(pr, "is_pi_process", lambda pid: False),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            self.assertEqual(pr.cmd_pick(), 0)
        self.assertEqual(json.loads(self.p.read_text()), {})

    def test_preview_fits_lines_but_last_round_full(self):
        f = self.root / "s.jsonl"
        old = "\n".join(f"old{i}" for i in range(40))
        write_session(f, [user("oldp"), asst(old), user("newp"), asst("newr")])
        self.p.write_text(json.dumps({"a": entry(file=str(f))}))
        env = {"FZF_PREVIEW_LINES": "10", "FZF_PREVIEW_COLUMNS": "80"}
        out = io.StringIO()
        with mock.patch.dict(os.environ, env), contextlib.redirect_stdout(out):
            pr.cmd_preview("a")
        text = out.getvalue()
        self.assertLessEqual(len(text.splitlines()), 10)
        self.assertIn("> newp", text)
        self.assertIn("newr", text)
        self.assertNotIn("old0", text)

    def test_preview_counts_wrapped_lines(self):
        f = self.root / "s.jsonl"
        write_session(f, [user("oldp"), asst("x" * 250), user("n"), asst("r")])
        self.p.write_text(json.dumps({"a": entry(file=str(f))}))
        env = {"FZF_PREVIEW_LINES": "10", "FZF_PREVIEW_COLUMNS": "20"}
        out = io.StringIO()
        with mock.patch.dict(os.environ, env), contextlib.redirect_stdout(out):
            pr.cmd_preview("a")
        self.assertNotIn("xxxx", out.getvalue())


class RestoreTests(unittest.TestCase):
    def setUp(self):
        t = tempfile.TemporaryDirectory()
        self.addCleanup(t.cleanup)
        self.root = Path(t.name)
        self.p = self.root / "restore.json"
        self.file = self.root / "sess.jsonl"
        self.file.write_text("{}\n")
        self.calls = []
        self.has_session = True
        env = mock.patch.dict(os.environ, {"PI_RESTORE_STATE": str(self.p)})
        env.start()
        self.addCleanup(env.stop)
        os.environ.pop("TMUX", None)
        os.environ.pop("REX_BLOCK", None)
        self.fail_cmd = None
        m = mock.patch.object(pr, "tmux", self.fake_tmux)
        m.start()
        self.addCleanup(m.stop)

    def fake_tmux(self, *args, capture=True):
        self.calls.append(args)
        if args[0] == self.fail_cmd:
            return mock.Mock(returncode=1, stdout="", stderr="boom")
        rc = 0 if (args[0] != "has-session" or self.has_session) else 1
        return mock.Mock(returncode=rc, stdout="@7\n", stderr="")

    def put(self, **kw):
        e = entry(file=str(self.file), cwd=str(self.root), tmuxSession="work", **kw)
        self.p.write_text(json.dumps({"id1": e}))

    def names(self):
        return [c[0] for c in self.calls]

    def test_existing_session_outside_tmux_attaches(self):
        self.put()
        self.assertEqual(pr.restore("id1"), 0)
        self.assertNotIn("new-session", self.names())
        nw = next(c for c in self.calls if c[0] == "new-window")
        self.assertEqual(nw[nw.index("-t") + 1], "=work:")
        self.assertEqual(nw[nw.index("-c") + 1], str(self.root))
        self.assertEqual(nw[-3:], ("pi", "--session", str(self.file)))
        self.assertEqual(self.calls[-1], ("attach-session", "-t", "=work:"))

    def test_missing_session_created_first(self):
        self.put()
        self.has_session = False
        pr.restore("id1")
        self.assertEqual(
            self.calls[1], ("new-session", "-d", "-s", "work", "-c", str(self.root))
        )
        self.assertLess(
            self.names().index("new-session"), self.names().index("new-window")
        )

    def test_targets_are_exact_match(self):
        e = entry(file=str(self.file), cwd=str(self.root), tmuxSession="a.b")
        self.p.write_text(json.dumps({"id1": e}))
        os.environ["TMUX"] = "/tmp/x,1,0"
        self.addCleanup(os.environ.pop, "TMUX", None)
        pr.restore("id1")
        t = {c[0]: c[c.index("-t") + 1] for c in self.calls if "-t" in c}
        self.assertEqual(t["has-session"], "=a.b:")
        self.assertEqual(t["new-window"], "=a.b:")
        self.assertEqual(t["switch-client"], "=a.b:")

    def test_failed_tmux_step_is_nonzero_with_error(self):
        for cmd in ("new-session", "new-window", "select-window", "switch-client"):
            with self.subTest(cmd=cmd):
                self.put()
                self.has_session = cmd != "new-session"
                self.fail_cmd = cmd
                os.environ["TMUX"] = "/tmp/x,1,0"
                self.addCleanup(os.environ.pop, "TMUX", None)
                err = io.StringIO()
                with contextlib.redirect_stderr(err):
                    rc = pr.restore("id1")
                self.assertNotEqual(rc, 0)
                self.assertIn("boom", err.getvalue())

    def test_inside_tmux_switches_client(self):
        self.put()
        os.environ["TMUX"] = "/tmp/x,1,0"
        self.addCleanup(os.environ.pop, "TMUX", None)
        pr.restore("id1")
        self.assertEqual(self.calls[-1][0], "switch-client")
        self.assertNotIn("attach-session", self.names())

    def test_null_session_uses_cwd_basename(self):
        e = entry(file=str(self.file), cwd=str(self.root), tmuxSession=None)
        self.p.write_text(json.dumps({"id1": e}))
        self.has_session = False
        pr.restore("id1")
        self.assertEqual(self.calls[1][3], self.root.name)  # new-session -s

    def test_missing_cwd_refuses_and_keeps_entry(self):
        e = entry(file=str(self.file), cwd=str(self.root / "gone"))
        self.p.write_text(json.dumps({"id1": e}))
        self.assertEqual(pr.restore("id1"), 1)
        self.assertEqual(self.calls, [])
        self.assertIn("id1", json.loads(self.p.read_text()))

    def test_missing_session_file_refuses_and_drops_entry(self):
        self.put()
        self.file.unlink()
        self.assertEqual(pr.restore("id1"), 1)
        self.assertEqual(self.calls, [])
        self.assertEqual(json.loads(self.p.read_text()), {})


class RexRestoreTests(unittest.TestCase):
    def setUp(self):
        t = tempfile.TemporaryDirectory()
        self.addCleanup(t.cleanup)
        self.root = Path(t.name)
        self.p = self.root / "restore.json"
        self.file = self.root / "sess.jsonl"
        self.file.write_text("{}\n")
        self.calls = []
        self.sessions = [{"session_id": "session:1", "label": "work"}]
        self.fail_cmd = None
        env = mock.patch.dict(
            os.environ,
            {"PI_RESTORE_STATE": str(self.p), "REX_BLOCK": "block:abc"},
        )
        env.start()
        self.addCleanup(env.stop)
        os.environ.pop("TMUX", None)
        os.environ.pop("REX_CLIENT", None)
        m = mock.patch.object(pr, "rex", self.fake_rex)
        m.start()
        self.addCleanup(m.stop)

    def fake_rex(self, *args):
        self.calls.append(args)
        key = " ".join(args[:2]) if args[0] == "window" else args[0]
        if key == self.fail_cmd:
            return mock.Mock(returncode=1, stdout="", stderr="rex boom")
        if args[0] == "ls":
            out = json.dumps({"sessions": self.sessions})
        elif args[:2] == ("window", "new"):
            out = json.dumps({"window_id": "window:9"})
        elif args[0] == "new":
            out = json.dumps({"session_id": "session:new"})
        else:
            out = "{}"
        return mock.Mock(returncode=0, stdout=out, stderr="")

    def put(self, **kw):
        e = entry(
            file=str(self.file),
            cwd=str(self.root),
            session="work",
            tmuxSession=None,
            mux="rex",
            **kw,
        )
        self.p.write_text(json.dumps({"id1": e}))

    def test_existing_session_opens_tab_and_selects_it(self):
        self.put()
        self.assertEqual(pr.restore("id1"), 0)
        wn = next(c for c in self.calls if c[:2] == ("window", "new"))
        self.assertEqual(wn[wn.index("-s") + 1], "session:1")
        self.assertEqual(wn[wn.index("-c") + 1], str(self.root))
        self.assertEqual(wn[-3:], ("pi", "--session", str(self.file)))
        self.assertEqual(
            self.calls[-1],
            ("do", "session.select", "session_id=session:1", "window_id=window:9"),
        )

    def test_missing_session_creates_and_selects_it(self):
        self.sessions = []
        self.put()
        self.assertEqual(pr.restore("id1"), 0)
        nc = next(c for c in self.calls if c[0] == "new")
        self.assertEqual(nc[1], "work")
        self.assertEqual(nc[nc.index("-c") + 1], str(self.root))
        self.assertEqual(nc[-3:], ("pi", "--session", str(self.file)))
        self.assertEqual(
            self.calls[-1], ("do", "session.select", "session_id=session:new")
        )

    def test_failed_rex_step_is_nonzero_with_error(self):
        for cmd in ("ls", "window new", "new", "do"):
            with self.subTest(cmd=cmd):
                self.calls.clear()
                self.sessions = (
                    []
                    if cmd == "new"
                    else [{"session_id": "session:1", "label": "work"}]
                )
                self.fail_cmd = cmd
                self.put()
                err = io.StringIO()
                with contextlib.redirect_stderr(err):
                    rc = pr.restore("id1")
                self.assertNotEqual(rc, 0)
                self.assertIn("rex boom", err.getvalue())

    def test_target_mux_precedence(self):
        # REX_BLOCK set, TMUX unset -> rex even for legacy tmux entry
        self.assertEqual(pr.target_mux({"tmuxSession": "aiv"}), "rex")
        # TMUX wins when nested inside Rex
        with mock.patch.dict(os.environ, {"TMUX": "/tmp/x,1,0"}):
            self.assertEqual(pr.target_mux({"mux": "rex"}), "tmux")
        # Outside both -> entry mux
        with mock.patch.dict(os.environ, {}, clear=True):
            self.assertEqual(pr.target_mux({"mux": "rex"}), "rex")
            self.assertEqual(pr.target_mux({"tmuxSession": "aiv"}), "tmux")

    def test_session_of_reads_new_and_legacy_field(self):
        self.assertEqual(pr.session_of({"session": "r", "tmuxSession": "t"}), "r")
        self.assertEqual(pr.session_of({"tmuxSession": "t"}), "t")
        self.assertIsNone(pr.session_of({}))


if __name__ == "__main__":
    unittest.main()
