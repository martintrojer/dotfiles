#!/usr/bin/env python3
"""Behavior tests for the apply policy in sync.run_apply_group.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

--ignore conflict:<rel> means "leave that target to me". It used to mean
"delete it without a backup" as soon as --force-overwrite was also passed:
the ignored link was dropped from the backup list but not from the apply
list. These tests pin both halves of the contract -- the ignored file keeps
its bytes, and a non-ignored conflict still gets backed up and linked.
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import sync
from _dotfiles_sync.model import PackageSpec


class ForceOverwriteIgnoreTests(unittest.TestCase):
    """A miniature repo/target pair; never $HOME."""

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        self.repo = root / "repo"
        self.target = root / "home"
        self.backup_root = self.target / ".dotfiles-sync-backups"
        pkg = self.repo / "pkg"
        pkg.mkdir(parents=True)
        (pkg / ".keepme").write_text("from repo\n")
        (pkg / ".takeme").write_text("from repo\n")
        self.target.mkdir()
        (self.target / ".keepme").write_text("precious\n")
        (self.target / ".takeme").write_text("stale local\n")
        self.specs = {
            "pkg": PackageSpec(name="pkg", stow_dir=self.repo, scope="common")
        }

    def apply(self, *, ignore: set[str]) -> None:
        sync.run_apply_group(
            "test",
            ["pkg"],
            self.specs,
            self.target,
            verbose=False,
            force_overwrite=True,
            backup_root=self.backup_root,
            ignore=ignore,
        )

    def test_ignored_conflict_is_untouched_under_force_overwrite(self) -> None:
        self.apply(ignore={"conflict:.keepme"})
        keepme = self.target / ".keepme"
        self.assertFalse(keepme.is_symlink(), "ignored conflict was replaced")
        self.assertEqual(keepme.read_text(), "precious\n")
        self.assertFalse(
            (self.backup_root / ".keepme").exists(),
            "ignored conflict should not be moved into the backup root",
        )

    def test_non_ignored_conflict_is_backed_up_and_linked(self) -> None:
        self.apply(ignore={"conflict:.keepme"})
        takeme = self.target / ".takeme"
        self.assertTrue(takeme.is_symlink())
        self.assertEqual(takeme.resolve(), (self.repo / "pkg" / ".takeme").resolve())
        self.assertEqual((self.backup_root / ".takeme").read_text(), "stale local\n")


if __name__ == "__main__":
    unittest.main()
