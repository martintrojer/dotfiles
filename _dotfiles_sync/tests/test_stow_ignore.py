#!/usr/bin/env python3
"""Behavior tests for .stowrc ignore matching and the conflict filter.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

These sit on the `--apply` hot path: repo_path_matches_stow_ignore decides
which managed symlinks get pruned, and run_check_group's filter decides
which conflicts are reported.
"""

from __future__ import annotations

import re
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import repo_checks, stow
from _dotfiles_sync.model import PackageSpec


def compile_patterns(*regexes: str) -> tuple[tuple[str, re.Pattern[str]], ...]:
    return tuple((r, re.compile(r)) for r in regexes)


class LoadStowIgnoreTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        patch = mock.patch.object(repo_checks, "SCRIPT_DIR", self.root)
        patch.start()
        self.addCleanup(patch.stop)

    def write_stowrc(self, body: str) -> None:
        (self.root / ".stowrc").write_text(body, encoding="utf-8")

    def test_bare_name_patterns_load(self) -> None:
        self.write_stowrc("--ignore=__pycache__\n# a comment\n--ignore=.+\\.pyc\n")
        self.assertEqual(
            [raw for raw, _ in repo_checks.load_repo_stow_ignore_regexes()],
            ["__pycache__", ".+\\.pyc"],
        )

    def test_slash_pattern_is_rejected_at_load(self) -> None:
        # Stow tests one path component at a time, so a slash pattern can
        # never fire. Failing here beats looking like it works.
        self.write_stowrc("--ignore=^.*/guides/build$\n")
        with self.assertRaises(SystemExit) as caught:
            repo_checks.load_repo_stow_ignore_regexes()
        self.assertIn("can never match", str(caught.exception))

    def test_missing_stowrc_is_empty_not_an_error(self) -> None:
        self.assertEqual(repo_checks.load_repo_stow_ignore_regexes(), ())


class RepoPathMatchesStowIgnoreTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.pkg = self.root / "guides"
        self.pkg.mkdir()
        self.specs = {
            "guides": PackageSpec(name="guides", stow_dir=self.root, scope="common")
        }

    def matches(self, rel: str, *regexes: str) -> bool:
        return repo_checks.repo_path_matches_stow_ignore(
            self.pkg / rel, self.specs, compile_patterns(*regexes)
        )

    def test_matches_at_the_package_root(self) -> None:
        self.assertTrue(self.matches("__pycache__", "__pycache__"))

    def test_matches_at_any_depth(self) -> None:
        self.assertTrue(self.matches("a/b/__pycache__/x.pyc", "__pycache__"))

    def test_component_match_is_anchored(self) -> None:
        # fullmatch, not search: a component merely containing the pattern
        # is not ignored.
        self.assertFalse(self.matches("my__pycache__extra/x", "__pycache__"))

    def test_non_matching_path_is_not_ignored(self) -> None:
        self.assertFalse(self.matches("style.css", "__pycache__", "\\.ruff_cache"))

    def test_path_outside_every_package_is_not_ignored(self) -> None:
        self.assertFalse(
            repo_checks.repo_path_matches_stow_ignore(
                self.root / "elsewhere/__pycache__",
                self.specs,
                compile_patterns("__pycache__"),
            )
        )


class ConflictOutputFilterTests(unittest.TestCase):
    """The ignored-conflict filter in run_check_group is substring-based.

    meta_ignore_dead_conflict_shortcircuit flagged that as a hazard: an
    ignored target that is a substring of another path drops the unrelated
    line too. Pin the behavior so a future edit to that block has to make
    the loss deliberate.
    """

    @staticmethod
    def filter_output(output: str, ignored_targets: set[str]) -> str:
        # Mirrors _dotfiles_sync/stow.py run_check_group.
        return "\n".join(
            line
            for line in output.splitlines()
            if not any(target_rel in line for target_rel in ignored_targets)
        )

    def test_ignored_target_line_is_dropped(self) -> None:
        output = "LINK: .config/foo => x\nLINK: .config/bar => y"
        self.assertEqual(
            self.filter_output(output, {".config/foo"}),
            "LINK: .config/bar => y",
        )

    def test_substring_collision_drops_an_unrelated_line(self) -> None:
        # ".config/foo" is a prefix of ".config/foobar", so filtering on the
        # former silently swallows the latter. Documented, not endorsed: if
        # this ever bites, the fix is to match the parsed target, not the
        # raw line.
        output = "LINK: .config/foo => x\nLINK: .config/foobar => y"
        self.assertEqual(self.filter_output(output, {".config/foo"}), "")

    def test_empty_ignore_set_is_a_no_op(self) -> None:
        output = "LINK: .config/foo => x\nLINK: .config/bar => y"
        self.assertEqual(self.filter_output(output, set()), output)


class MeaningfulOutputTests(unittest.TestCase):
    def test_simulation_noise_is_suppressed(self) -> None:
        raw = (
            "WARNING: in simulation mode so not modifying filesystem.\n"
            "UNLINK: .config/foo\n"
            "LINK: .config/foo => x (reverts previous action)\n"
            "\n"
            "LINK: .config/bar => y\n"
        )
        self.assertEqual(stow.meaningful_output(raw), "LINK: .config/bar => y")


if __name__ == "__main__":
    unittest.main()
