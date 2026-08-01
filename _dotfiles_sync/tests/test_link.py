"""Behaviour tests for the symlink planner that replaced GNU Stow.

These sit on the `--apply` hot path: a wrong plan writes or deletes symlinks in
a live $HOME. Each test pins a property the stow implementation gave us for
free and that we now own.
"""

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync.link import (
    Link,
    LinkState,
    apply_link,
    plan_package,
)


class PlanPackageTests(unittest.TestCase):
    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.root = Path(self._tmp.name)
        self.pkg = self.root / "pkg"
        self.home = self.root / "home"
        self.pkg.mkdir()
        self.home.mkdir()
        self.addCleanup(self._tmp.cleanup)

    def _write(self, rel: str, body: str = "x") -> Path:
        path = self.pkg / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(body)
        return path

    def _plan(self, bundle_dirs: tuple[Path, ...] = ()):
        return plan_package(
            "pkg",
            self.pkg,
            self.home,
            bundle_dirs=bundle_dirs,
            repo_root=self.root,
        )

    def test_per_leaf_links_files_not_directories(self) -> None:
        """Parents stay real dirs so two packages can share a target dir."""
        self._write(".config/app/conf.toml")
        rels = sorted(link.rel for link in self._plan())
        self.assertEqual(rels, [".config/app/conf.toml"])

    def test_missing_target_is_actionable(self) -> None:
        self._write(".zshrc")
        (link,) = self._plan()
        self.assertIs(link.state, LinkState.MISSING)
        self.assertTrue(link.is_actionable)

    def test_existing_correct_link_is_ok_and_idempotent(self) -> None:
        self._write(".zshrc")
        (link,) = self._plan()
        apply_link(link)
        (again,) = self._plan()
        self.assertIs(again.state, LinkState.OK)
        self.assertFalse(again.is_actionable)

    def test_foreign_file_is_a_conflict_not_overwritten(self) -> None:
        self._write(".zshrc")
        (self.home / ".zshrc").write_text("theirs")
        (link,) = self._plan()
        self.assertIs(link.state, LinkState.CONFLICT)
        self.assertFalse(link.is_actionable)
        self.assertEqual((self.home / ".zshrc").read_text(), "theirs")

    def test_link_to_a_moved_repo_path_is_stale(self) -> None:
        """An intra-repo move leaves our link pointing at a path that is gone.

        The source still exists at its new location, so the planner sees it and
        can relink. A source deleted outright drops out of the plan entirely --
        cleaning that up belongs to the backlink audit in repo_checks, which
        walks $HOME rather than the package.
        """
        self._write("old-name")
        (link,) = self._plan()
        apply_link(link)
        (self.pkg / "old-name").rename(self.pkg / "new-name")
        # The stale link is still on disk at the old target path.
        stale = self.home / "old-name"
        self.assertTrue(stale.is_symlink())
        self.assertFalse(stale.exists())
        # And the plan now wants the new name.
        rels = sorted(link.rel for link in self._plan())
        self.assertEqual(rels, ["new-name"])

    def test_our_dangling_link_is_stale_and_replaceable(self) -> None:
        """A link into the repo at a dead path is ours to fix, not a conflict."""
        self._write(".zshrc")
        (self.home / ".zshrc").symlink_to(self.pkg / "gone-away")
        (link,) = self._plan()
        self.assertIs(link.state, LinkState.STALE)
        self.assertTrue(link.is_actionable)
        apply_link(link)
        self.assertEqual(
            (self.home / ".zshrc").resolve(), (self.pkg / ".zshrc").resolve()
        )

    def test_link_outside_the_repo_is_a_conflict_not_stale(self) -> None:
        """Someone else's dangling symlink must not be silently replaced."""
        self._write(".zshrc")
        (self.home / ".zshrc").symlink_to("/nowhere/at/all")
        (link,) = self._plan()
        self.assertIs(link.state, LinkState.CONFLICT)

    def test_ignored_names_never_reach_the_plan(self) -> None:
        self._write(".config/app/conf.toml")
        self._write("README.md")
        self._write("__pycache__/x.pyc")
        self._write("scripts/tests/test_x.py")
        rels = sorted(link.rel for link in self._plan())
        self.assertEqual(rels, [".config/app/conf.toml"])

    def test_bundle_dir_children_link_as_directories(self) -> None:
        """skills/: each child is one opaque link, vendored files ride along."""
        self._write(".agents/skills/alpha/SKILL.md")
        self._write(".agents/skills/alpha/README.md")
        self._write(".agents/skills/beta/SKILL.md")
        links = self._plan(bundle_dirs=(Path(".agents/skills"),))
        rels = sorted(link.rel for link in links)
        self.assertEqual(rels, [".agents/skills/alpha", ".agents/skills/beta"])
        # README.md is ignored per-leaf, but must survive inside a bundle.
        apply_link(next(link for link in links if link.rel.endswith("alpha")))
        self.assertTrue((self.home / ".agents/skills/alpha/README.md").is_file())

    def test_apply_replaces_a_stale_link_and_makes_parents(self) -> None:
        self._write(".config/deep/nested/file")
        (link,) = self._plan()
        apply_link(link)
        self.assertTrue((self.home / ".config/deep/nested/file").is_symlink())
        self.assertTrue((self.home / ".config/deep").is_dir())
        self.assertFalse((self.home / ".config/deep").is_symlink())


class ApplyLinkDirectoryTests(unittest.TestCase):
    """apply_link is the only function here that deletes user data.

    No package pair in the current inventory reaches it with a real directory --
    the conflict/backup policy in sync.py gets there first. It stays reachable in
    principle because a group's plan is computed before any of it is applied, so
    one link's mkdir can turn a path another link planned as MISSING into a
    directory. Refuse with an actionable message, not IsADirectoryError.
    """

    def setUp(self) -> None:
        self._tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self._tmp.cleanup)
        root = Path(self._tmp.name)
        self.source = root / "repo" / "pkg" / ".conf"
        self.source.parent.mkdir(parents=True)
        self.source.write_text("from repo\n")
        self.home = root / "home"
        self.home.mkdir()

    def _link(self) -> Link:
        return Link(
            package="pkg",
            source=self.source,
            target=self.home / ".conf",
            state=LinkState.MISSING,
            rel=".conf",
        )

    def test_directory_target_is_refused_with_a_clear_message(self) -> None:
        target = self.home / ".conf"
        target.mkdir()
        (target / "inner").write_text("precious\n")

        with self.assertRaises(SystemExit) as caught:
            apply_link(self._link())

        message = str(caught.exception)
        self.assertIn("refusing to replace directory", message)
        self.assertIn(str(target), message)
        self.assertEqual(
            (target / "inner").read_text(), "precious\n", "refusal must not delete"
        )
        self.assertFalse(target.is_symlink())

    def test_symlink_to_a_directory_is_still_replaced(self) -> None:
        """is_dir() follows symlinks; only a *real* directory is refused."""
        elsewhere = Path(self._tmp.name) / "elsewhere"
        elsewhere.mkdir()
        target = self.home / ".conf"
        target.symlink_to(elsewhere)

        apply_link(self._link())

        self.assertTrue(target.is_symlink())
        self.assertEqual(target.resolve(), self.source.resolve())
        self.assertTrue(elsewhere.is_dir(), "the pointed-at directory must survive")


if __name__ == "__main__":
    unittest.main()
