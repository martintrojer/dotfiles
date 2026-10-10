#!/usr/bin/env python3
"""Regression tests for forgejo-sync's silent-failure paths.

A fork judged "no commits of mine" is skipped quietly (hidden without -v,
exit 0) and never backed up, so a wrong verdict loses data unnoticed.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).parents[1] / "forgejo-sync"


def load():
    loader = importlib.machinery.SourceFileLoader("forgejo_sync", str(SCRIPT))
    spec = importlib.util.spec_from_loader("forgejo_sync", loader)
    assert spec
    mod = importlib.util.module_from_spec(spec)
    sys.modules["forgejo_sync"] = mod
    loader.exec_module(mod)
    return mod


fs = load()

# Fake gh: looks up "<api path>" in $FAKE_GH (JSON: path -> [rc, stdout,
# stderr]) and ignores --jq/--paginate; tests store pre-filtered output.
FAKE_GH = """#!/usr/bin/env python3
import json, os, sys
table = json.load(open(os.environ["FAKE_GH"]))
path = sys.argv[2]
rc, out, err = table.get(path, [1, "", "gh: Not Found (HTTP 404)"])
sys.stdout.write(out); sys.stderr.write(err); sys.exit(rc)
"""

PARENT = "repos/me/fork"
BRANCHES = "repos/me/fork/branches?per_page=100"


def compare(base: str, head: str) -> str:
    return f"repos/me/fork/compare/up:{base}...{head}?per_page=100"


class ForkHasMyWorkTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        gh = root / "gh"
        gh.write_text(FAKE_GH)
        gh.chmod(0o755)
        self.table_file = root / "table.json"
        old = os.environ.copy()
        self.addCleanup(lambda: (os.environ.clear(), os.environ.update(old)))
        os.environ["PATH"] = f"{root}:{os.environ['PATH']}"
        os.environ["FAKE_GH"] = str(self.table_file)

    def gh(self, table: dict[str, list]) -> None:
        base = {
            PARENT: [0, "up\tmain\n", ""],
            BRANCHES: [0, '"main"\n', ""],
        }
        self.table_file.write_text(json.dumps(base | table))

    def test_compare_failure_is_an_error_not_a_skip(self) -> None:
        self.gh(
            {compare("main", "main"): [1, "", "gh: API rate limit exceeded (HTTP 403)"]}
        )
        with self.assertRaises(fs.GhError):
            fs.fork_has_my_work("me", "fork")

    def test_my_commit_keeps_the_fork(self) -> None:
        self.gh({compare("main", "main"): [0, "1\n", ""]})
        self.assertTrue(fs.fork_has_my_work("me", "fork"))

    def test_no_commits_of_mine_skips(self) -> None:
        self.gh({compare("main", "main"): [0, "0\n", ""]})
        self.assertFalse(fs.fork_has_my_work("me", "fork"))

    def test_counts_every_page(self) -> None:
        # --paginate prints one count per page; mine are on the second.
        self.gh({compare("main", "main"): [0, "0\n2\n", ""]})
        self.assertTrue(fs.fork_has_my_work("me", "fork"))

    def test_branch_missing_in_parent_falls_back_to_default(self) -> None:
        self.gh(
            {
                BRANCHES: [0, '"topic"\n', ""],
                compare("main", "topic"): [0, "3\n", ""],
            }
        )
        self.assertTrue(fs.fork_has_my_work("me", "fork"))

    def test_unrelated_history_counts_branch_commits(self) -> None:
        self.gh(
            {
                BRANCHES: [0, '"gh-pages"\n', ""],
                compare("gh-pages", "gh-pages"): [1, "", "gh: Not Found (HTTP 404)"],
                compare("main", "gh-pages"): [
                    1,
                    "",
                    "gh: No common ancestor between up:main and gh-pages. (HTTP 404)",
                ],
                "repos/me/fork/commits?sha=gh-pages&per_page=100": [0, "1\n", ""],
            }
        )
        self.assertTrue(fs.fork_has_my_work("me", "fork"))

    def test_branch_name_is_url_encoded(self) -> None:
        self.gh(
            {
                BRANCHES: [0, '"fix#1"\n', ""],
                compare("main", "fix%231"): [0, "1\n", ""],
            }
        )
        self.assertTrue(fs.fork_has_my_work("me", "fork"))


class GitParsingTests(unittest.TestCase):
    """Real git output, so a format change in git fails here, not silently."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        env = {"GIT_CONFIG_GLOBAL": "/dev/null", "GIT_CONFIG_NOSYSTEM": "1"}
        self.env = os.environ | env

        def git(*args: str, cwd: Path = self.root) -> str:
            return subprocess.run(
                ["git", "-c", "user.name=t", "-c", "user.email=t@t", *args],
                cwd=cwd, env=self.env, check=True, capture_output=True, text=True,
            ).stdout  # fmt: skip

        self.git = git
        git("init", "-q", "--bare", "remote.git")
        git("init", "-q", "-b", "main", "work")
        self.work = self.root / "work"
        git("commit", "-q", "--allow-empty", "-m", "a", cwd=self.work)
        git("branch", "odd\u00a0name", cwd=self.work)
        git("tag", "-a", "v1", "-m", "v1", cwd=self.work)

    def test_refs_keeps_names_with_unicode_spaces(self) -> None:
        got = fs.refs(
            ["git", "for-each-ref", "--format=%(objectname)%09%(refname)"], self.work
        )
        self.assertIn("refs/heads/odd\u00a0name", got)
        self.assertIn("refs/tags/v1", got)

    def test_parse_push_tells_divergence_from_server_rejection(self) -> None:
        remote = str(self.root / "remote.git")
        self.git("push", "-q", remote, "main", "v1", cwd=self.work)
        hook = self.root / "remote.git" / "hooks" / "update"
        hook.write_text('#!/bin/sh\n[ "$1" = refs/heads/blocked ] && exit 1\nexit 0\n')
        hook.chmod(0o755)
        self.git("commit", "-q", "--allow-empty", "--amend", "-m", "b", cwd=self.work)
        self.git("branch", "blocked", cwd=self.work)
        self.git("branch", "new", cwd=self.work)
        p = subprocess.run(
            ["git", "push", "--porcelain", remote,
             "refs/heads/main:refs/heads/main",
             "refs/heads/blocked:refs/heads/blocked",
             "refs/heads/new:refs/heads/new"],
            cwd=self.work, env=self.env, capture_output=True, text=True, check=False,
        )  # fmt: skip
        o = fs.parse_push(p.stdout)
        self.assertEqual(o.diverged, ["refs/heads/main"])
        self.assertEqual(o.updated, ["refs/heads/new"])
        self.assertEqual(len(o.rejected), 1)
        self.assertIn("blocked", o.rejected[0])

    def test_parse_push_reports_forced_alongside_rejection(self) -> None:
        o = fs.parse_push(
            "To x\n"
            "+\trefs/heads/a:refs/heads/a\t1...2 (forced update)\n"
            "!\trefs/heads/b:refs/heads/b\t[remote rejected] (hook declined)\n"
            "Done\n"
        )
        self.assertEqual(o.forced, ["refs/heads/a"])
        self.assertEqual(o.updated, ["refs/heads/a"])
        self.assertEqual(o.diverged, [])
        self.assertEqual(len(o.rejected), 1)


class ConfigPathTests(unittest.TestCase):
    def test_empty_xdg_falls_back_to_home(self) -> None:
        old = os.environ.get("XDG_CONFIG_HOME")
        os.environ["XDG_CONFIG_HOME"] = ""
        try:
            self.assertEqual(
                fs.xdg("XDG_CONFIG_HOME", ".config"),
                Path.home() / ".config" / "forgejo-sync",
            )
        finally:
            if old is None:
                del os.environ["XDG_CONFIG_HOME"]
            else:
                os.environ["XDG_CONFIG_HOME"] = old


if __name__ == "__main__":
    unittest.main()
