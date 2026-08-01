#!/usr/bin/env python3
"""Behavior tests for the managed-symlink walk shared by prune and check.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

iter_managed_links is the traversal both `--apply` and `--check` run over
the whole target tree. Its iteration order is load-bearing: the prune step
walks path.parents alongside repo_target.parents.
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import config, repo_checks
from _dotfiles_sync.model import PackageSpec


class IterManagedLinksTests(unittest.TestCase):
    """A miniature stow layout: repo/pkg/... symlinked into target/."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        self.repo = root / "repo"
        self.target = root / "home"
        (self.repo / "pkg" / ".config" / "app").mkdir(parents=True)
        self.target.mkdir()

        patch = mock.patch.object(repo_checks, "SCRIPT_DIR", self.repo)
        patch.start()
        self.addCleanup(patch.stop)

        self.specs = {
            "pkg": PackageSpec(name="pkg", stow_dir=self.repo, scope="common")
        }
        self.active = {"pkg"}

    def repo_file(self, rel: str) -> Path:
        path = self.repo / "pkg" / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("x", encoding="utf-8")
        return path

    def link(self, rel: str, to: Path) -> Path:
        path = self.target / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.symlink_to(to)
        return path

    def walk(self) -> list[tuple[Path, Path]]:
        return list(
            repo_checks.iter_managed_links(self.target, self.specs, self.active)
        )

    def test_nested_managed_link_is_found(self) -> None:
        src = self.repo_file(".config/app/conf")
        dest = self.link(".config/app/conf", src)
        self.assertEqual(self.walk(), [(dest, src.resolve())])

    def test_link_pointing_outside_the_repo_is_skipped(self) -> None:
        outside = self.target.parent / "elsewhere"
        outside.write_text("x", encoding="utf-8")
        self.link(".config/stray", outside)
        self.assertEqual(self.walk(), [])

    def test_broken_link_into_the_repo_is_still_yielded(self) -> None:
        # This is the stale case check_repo_backlinks reports; the walk must
        # not filter it out on the way.
        missing = self.repo / "pkg" / ".config/app/gone"
        dest = self.link(".config/app/gone", missing)
        self.assertEqual(self.walk(), [(dest, missing.resolve())])

    def test_a_scan_root_that_is_itself_a_link_is_yielded(self) -> None:
        src = self.repo_file(".config/app/conf")
        dest = self.link(".config", src.parent)
        self.assertEqual(self.walk(), [(dest, src.parent.resolve())])

    def test_plain_files_and_dirs_are_not_yielded(self) -> None:
        self.repo_file(".config/app/conf")
        (self.target / ".config" / "app").mkdir(parents=True)
        (self.target / ".config" / "app" / "real").write_text("x", encoding="utf-8")
        self.assertEqual(self.walk(), [])

    def test_parents_align_for_the_prune_step(self) -> None:
        # prune_managed_ignored_artifact_links zips path.parents against
        # repo_target.parents; the pair must agree component-for-component.
        src = self.repo_file(".config/app/__pycache__/x.pyc")
        dest = self.link(".config/app/__pycache__/x.pyc", src)
        (link_path, repo_target) = self.walk()[0]
        self.assertEqual(link_path, dest)
        pairs = list(zip(link_path.parents, repo_target.parents, strict=False))
        self.assertEqual(
            [(a.name, b.name) for a, b in pairs[:3]],
            [("__pycache__", "__pycache__"), ("app", "app"), (".config", ".config")],
        )


class LazyHeaderTests(unittest.TestCase):
    """Sections announce themselves once, and only if they have something to say."""

    def test_header_is_not_printed_until_called(self) -> None:
        with self.assertNoLogs("dotfiles-sync", level="WARNING"):
            config.lazy_header("quiet-section")

    def test_header_is_printed_once_however_many_calls(self) -> None:
        print_header = config.lazy_header("noisy-section")
        with self.assertLogs("dotfiles-sync", level="WARNING") as caught:
            print_header()
            print_header()
            print_header()
        self.assertEqual(caught.output, ["WARNING:dotfiles-sync:\n[noisy-section]"])

    def test_each_header_latches_independently(self) -> None:
        first = config.lazy_header("one")
        second = config.lazy_header("two")
        with self.assertLogs("dotfiles-sync", level="WARNING") as caught:
            first()
            second()
            first()
        self.assertEqual(
            caught.output,
            ["WARNING:dotfiles-sync:\n[one]", "WARNING:dotfiles-sync:\n[two]"],
        )


if __name__ == "__main__":
    unittest.main()
