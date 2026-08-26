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


class ManagedLinkLayout(unittest.TestCase):
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


class IterManagedLinksTests(ManagedLinkLayout):
    """What the shared traversal does and does not yield."""

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


class ScanScopeTests(ManagedLinkLayout):
    """The walk covers mirrored directories and nothing else.

    collect_scan_roots used to hand back top-level roots for a recursive
    rglob, so a managed link under `~/.local/bin` meant descending every
    unmanaged sibling tree -- 450k entries on this Linux host, ~11s a run.
    """

    def test_link_in_a_mirrored_dir_is_found_without_walking_siblings(self) -> None:
        src = self.repo_file(".local/bin/tool")
        dest = self.link(".local/bin/tool", src)

        # A large unmanaged tree beside the mirrored dir, as ~/.local/share is.
        bulk = self.target / ".local" / "share" / "depot"
        bulk.mkdir(parents=True)
        for index in range(50):
            (bulk / f"blob{index}").write_text("x", encoding="utf-8")

        self.assertEqual(self.walk(), [(dest, src.resolve())])

    def test_unmanaged_subtree_under_a_mirrored_dir_is_not_descended(self) -> None:
        self.repo_file(".config/app/conf")
        stray = self.target / ".config" / "app" / "cache" / "deep"
        stray.mkdir(parents=True)
        # A link into the repo, but buried where the planner never puts one.
        stray_link = stray / "conf"
        stray_link.symlink_to(self.repo / "pkg" / ".config" / "app" / "conf")
        self.assertEqual(self.walk(), [])

    def test_link_in_a_deleted_mirrored_subdir_is_found(self) -> None:
        # Deleting a whole mirrored *directory* leaves no repo dir to derive a
        # scan root from, so its contents were invisible: the parent root is
        # read one level deep and the link sits two. A removed Sunshine unit
        # drop-in dangled in $HOME with --check reporting a clean tree.
        self.repo_file(".config/systemd/user/keep.service")
        gone = self.repo / "pkg" / ".config/systemd/user/gone.service.d/override.conf"
        dest = self.link(".config/systemd/user/gone.service.d/override.conf", gone)
        self.assertEqual(self.walk(), [(dest, gone.resolve())])

    def test_link_in_a_deeply_deleted_mirrored_subdir_is_found(self) -> None:
        # Same failure, more nesting: descent must follow the managed links
        # down to MAX_ORPHAN_DEPTH.
        self.repo_file(".config/app/conf")
        gone = self.repo / "pkg" / ".config/app/a/b/leaf"
        dest = self.link(".config/app/a/b/leaf", gone)
        self.assertEqual(self.walk(), [(dest, gone.resolve())])

    def test_descent_into_unmirrored_dirs_is_depth_bounded(self) -> None:
        # The bound is what keeps this walk off ~/.local/share and ~/.var/app,
        # whose sizes are nothing to do with the repo. Losing it would restore
        # the 450k-entry walk collect_scan_roots exists to avoid, so the limit
        # is asserted rather than left to the stop-on-real-file heuristic.
        self.repo_file(".config/app/conf")
        depth = repo_checks.MAX_ORPHAN_DEPTH
        too_deep = self.repo / "pkg" / ".config/app" / Path(*"x" * (depth + 1)) / "leaf"
        self.link(str(too_deep.relative_to(self.repo / "pkg")), too_deep)
        self.assertEqual(self.walk(), [])

    def test_live_link_in_an_unmirrored_dir_is_left_to_its_owner(self) -> None:
        # A resolvable link in a dir the repo doesn't mirror was never put
        # there by the planner. Yielding it would offer somebody else's link
        # to the prune step.
        src = self.repo_file(".config/app/conf")
        stray = self.target / ".config" / "app" / "vendor"
        stray.mkdir(parents=True)
        (stray / "conf").symlink_to(src)
        self.assertEqual(self.walk(), [])

    def test_descent_stops_at_a_directory_holding_real_files(self) -> None:
        self.repo_file(".config/app/conf")
        owned = self.target / ".config" / "app" / "vendor"
        owned.mkdir(parents=True)
        (owned / "real.db").write_text("x", encoding="utf-8")
        gone = self.repo / "pkg" / ".config/app/vendor/override"
        (owned / "override").symlink_to(gone)
        self.assertEqual(self.walk(), [])

    def test_bundle_dir_contents_are_not_scanned(self) -> None:
        # A bundle links as one directory symlink, so scanning inside it walks
        # back into the repo. Any symlink a vendored tree happens to contain
        # would then be reported as a managed link in $HOME -- and offered to
        # the prune step, whose unlink() would land on the repo's own file.
        self.specs["pkg"] = PackageSpec(
            name="pkg",
            stow_dir=self.repo,
            scope="common",
            bundle_dirs=(Path(".agents/skills"),),
        )
        src = self.repo_file(".agents/skills/one/SKILL.md")
        (src.parent / "alias").symlink_to(src)
        bundle = src.parent.parent
        dest = self.link(".agents/skills", bundle)
        self.assertEqual(self.walk(), [(dest, bundle.resolve())])

    def test_link_to_a_deleted_bundle_child_is_found(self) -> None:
        # Bundle children link one-per-directory into the bundle dir, so the
        # bundle dir must itself be a scan root. It wasn't: deleting a skill
        # left ~/.agents/skills/<name> dangling, invisible to --check and
        # unreachable by the prune step, forever.
        self.specs["pkg"] = PackageSpec(
            name="pkg",
            stow_dir=self.repo,
            scope="common",
            bundle_dirs=(Path(".agents/skills"),),
        )
        self.repo_file(".agents/skills/kept/SKILL.md")
        gone = self.repo / "pkg" / ".agents" / "skills" / "deleted"
        dest = self.link(".agents/skills/deleted", gone)
        self.assertEqual(self.walk(), [(dest, gone.resolve())])


class PruneStaleManagedLinksTests(ManagedLinkLayout):
    """Deleting a file from the repo must not strand its link in $HOME.

    run_apply_group only touches links that are still in a package's plan, so
    a deleted repo file produced a dangling symlink that --check reported
    forever and no --apply could clear.
    """

    def prune(self) -> None:
        repo_checks.prune_stale_managed_links(self.target, self.specs, self.active)

    def test_dangling_link_into_the_repo_is_removed(self) -> None:
        missing = self.repo / "pkg" / ".config" / "app" / "deleted"
        dest = self.link(".config/app/deleted", missing)
        self.prune()
        self.assertFalse(dest.is_symlink())

    def test_live_link_is_left_alone(self) -> None:
        src = self.repo_file(".config/app/conf")
        dest = self.link(".config/app/conf", src)
        self.prune()
        self.assertTrue(dest.is_symlink())
        self.assertEqual(dest.resolve(), src.resolve())

    def test_dangling_link_in_a_deleted_mirrored_subdir_is_removed(self) -> None:
        # The end-to-end bug: --apply left this behind and --check called the
        # tree clean, so no run could ever resolve it.
        self.repo_file(".config/systemd/user/keep.service")
        missing = (
            self.repo / "pkg" / ".config/systemd/user/gone.service.d/override.conf"
        )
        dest = self.link(".config/systemd/user/gone.service.d/override.conf", missing)
        self.prune()
        self.assertFalse(dest.is_symlink())

    def test_dangling_link_outside_the_repo_is_left_alone(self) -> None:
        # Not ours to clean up, however broken it looks.
        dest = self.link(".config/stray", self.target.parent / "nowhere")
        self.prune()
        self.assertTrue(dest.is_symlink())

    def test_pruning_reports_what_it_removed(self) -> None:
        missing = self.repo / "pkg" / ".config" / "app" / "deleted"
        self.link(".config/app/deleted", missing)
        with self.assertLogs("dotfiles-sync", level="WARNING") as caught:
            self.prune()
        self.assertIn("CLEARED STALE: .config/app/deleted", "\n".join(caught.output))

    def test_a_clean_tree_prunes_nothing_and_says_nothing(self) -> None:
        src = self.repo_file(".config/app/conf")
        self.link(".config/app/conf", src)
        with self.assertNoLogs("dotfiles-sync", level="WARNING"):
            self.prune()


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
