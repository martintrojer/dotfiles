#!/usr/bin/env python3
"""Behavior tests for the apply policy in sync.run_apply_group.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

link.py plans links; this module decides what to *do* about them -- honour
--ignore, back up under --force-overwrite, skip a package rather than
half-link it. The planner was well covered and the policy layer that deletes
things was not, which is exactly where two data-loss bugs lived. Each test
here pins one policy rule against a tempfile target root, never $HOME.
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import link as link_module
from _dotfiles_sync import sync
from _dotfiles_sync.model import Overwrite, PackageSpec


class ApplyPolicyTestCase(unittest.TestCase):
    """A miniature repo/target pair. Nothing here touches the real $HOME.

    plan_package classifies STALE relative to the repo root, which defaults to
    the real checkout, so SCRIPT_DIR is pointed at the fake repo for the whole
    case -- otherwise a dangling link inside the fixture reads as CONFLICT.
    """

    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        root = Path(tmp.name)
        self.repo = root / "repo"
        self.target = root / "home"
        self.backup_root = self.target / ".dotfiles-sync-backups"
        self.repo.mkdir()
        self.target.mkdir()

        patch = mock.patch.object(link_module, "SCRIPT_DIR", self.repo)
        patch.start()
        self.addCleanup(patch.stop)

        self.specs: dict[str, PackageSpec] = {}

    def add_package(self, name: str, files: dict[str, str]) -> Path:
        """Create a repo package with `files` (package-relative -> contents)."""
        package_dir = self.repo / name
        for rel, body in files.items():
            path = package_dir / rel
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(body)
        self.specs[name] = PackageSpec(name=name, stow_dir=self.repo, scope="common")
        return package_dir

    def apply(
        self,
        packages: list[str],
        *,
        force_overwrite: bool = False,
        ignore: set[str] | None = None,
    ) -> None:
        sync.run_apply_group(
            "test",
            packages,
            self.specs,
            self.target,
            verbose=False,
            overwrite=Overwrite(backup_root=self.backup_root)
            if force_overwrite
            else None,
            ignore=ignore or set(),
        )

    def assertLinksTo(self, rel: str, source: Path) -> None:
        target = self.target / rel
        self.assertTrue(target.is_symlink(), f"{rel} is not a symlink")
        self.assertEqual(target.resolve(), source.resolve())


class ForceOverwriteIgnoreTests(ApplyPolicyTestCase):
    """--ignore conflict:<rel> means "leave that target to me"."""

    def setUp(self) -> None:
        super().setUp()
        self.pkg = self.add_package(
            "pkg", {".keepme": "from repo\n", ".takeme": "from repo\n"}
        )
        (self.target / ".keepme").write_text("precious\n")
        (self.target / ".takeme").write_text("stale local\n")

    def test_ignored_conflict_is_untouched_under_force_overwrite(self) -> None:
        self.apply(["pkg"], force_overwrite=True, ignore={"conflict:.keepme"})
        keepme = self.target / ".keepme"
        self.assertFalse(keepme.is_symlink(), "ignored conflict was replaced")
        self.assertEqual(keepme.read_text(), "precious\n")
        self.assertFalse(
            (self.backup_root / ".keepme").exists(),
            "ignored conflict should not be moved into the backup root",
        )

    def test_non_ignored_conflict_is_backed_up_and_linked(self) -> None:
        self.apply(["pkg"], force_overwrite=True, ignore={"conflict:.keepme"})
        self.assertLinksTo(".takeme", self.pkg / ".takeme")
        self.assertEqual((self.backup_root / ".takeme").read_text(), "stale local\n")


class ConflictWithoutForceTests(ApplyPolicyTestCase):
    """Without --force-overwrite a conflict blocks its package, whole."""

    def setUp(self) -> None:
        super().setUp()
        self.pkg = self.add_package(
            "pkg", {".conflicted": "from repo\n", ".clean": "from repo\n"}
        )
        (self.target / ".conflicted").write_text("precious\n")

    def test_conflicting_target_is_left_untouched(self) -> None:
        self.apply(["pkg"])
        conflicted = self.target / ".conflicted"
        self.assertFalse(conflicted.is_symlink())
        self.assertEqual(conflicted.read_text(), "precious\n")

    def test_whole_package_is_skipped_not_half_linked(self) -> None:
        """The package's other links are held back too, rather than partly applied."""
        self.apply(["pkg"])
        self.assertFalse(
            (self.target / ".clean").exists(),
            "package with a conflict must not be half-linked",
        )

    def test_no_backup_root_is_created(self) -> None:
        self.apply(["pkg"])
        self.assertFalse(self.backup_root.exists())


class SiblingPackageTests(ApplyPolicyTestCase):
    """One package's conflict must not hold up the others in the group."""

    def setUp(self) -> None:
        super().setUp()
        self.blocked = self.add_package("blocked", {".blocked": "from repo\n"})
        self.sibling = self.add_package("sibling", {".sibling": "from repo\n"})
        (self.target / ".blocked").write_text("precious\n")

    def test_only_the_conflicting_package_is_skipped(self) -> None:
        self.apply(["blocked", "sibling"])
        self.assertEqual((self.target / ".blocked").read_text(), "precious\n")
        self.assertLinksTo(".sibling", self.sibling / ".sibling")


class StaleRelinkTests(ApplyPolicyTestCase):
    """A STALE link is ours and its destination is gone: relink, no backup."""

    def setUp(self) -> None:
        super().setUp()
        self.pkg = self.add_package("pkg", {".zshrc": "from repo\n"})
        self.stale_dest = self.repo / "pkg" / ".gone"
        (self.target / ".zshrc").symlink_to(self.stale_dest)

    def test_stale_link_is_relinked(self) -> None:
        self.apply(["pkg"])
        self.assertLinksTo(".zshrc", self.pkg / ".zshrc")

    def test_stale_link_is_not_treated_as_a_conflict(self) -> None:
        """It must not block the package, and there is nothing to back up."""
        self.apply(["pkg"])
        self.assertFalse(self.backup_root.exists())


if __name__ == "__main__":
    unittest.main()
