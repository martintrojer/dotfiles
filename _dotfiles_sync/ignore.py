"""Which repo paths never become symlinks.

Previously this lived in two places: `.stowrc` (consumed by stow) and a
hand-written reimplementation of stow's matching semantics in repo_checks, so
the backlink audit could agree with what stow had actually linked. Two
implementations of one rule set, kept in sync by hand.

Now there is one. The rules are declared here rather than in a dotfile because
nothing external reads them any more, and being Python means the awkward part
of stow's design goes away: stow matched each `--ignore` regex against a single
path *component*, so a pattern containing "/" silently never fired. We match
components for bare names and full relative paths for anything with a slash,
which is what a reader expects.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from functools import cache
from pathlib import Path
from typing import Final

# Matched against any single path component, at any depth.
NAME_PATTERNS: Final[tuple[str, ...]] = (
    r"__pycache__",
    r".+\.pyc",
    r"\.ruff_cache",
    r"\.pytest_cache",
    r"\.mypy_cache",
    r"\.tox",
    r"\.nox",
    r"\.venv",
    r"\.direnv",
    r"\.DS_Store",
    r"CACHEDIR\.TAG",
    # Focused regression suites live next to the scripts they cover, because
    # those scripts rely on sys.path[0] colocation. They are repo
    # infrastructure, not dotfiles: never link them into $HOME.
    r"tests",
    # Repo-level docs and vendored licence files. A package that genuinely
    # needs one in $HOME (skills/) links its subtree whole, which bypasses
    # this walk entirely.
    r"README\.md",
    r"LICENSE.*",
    r"COPYING",
)


@dataclass(frozen=True)
class IgnoreRules:
    names: tuple[re.Pattern[str], ...]

    @classmethod
    @cache
    def load(cls) -> IgnoreRules:
        """The compiled rule set. Cached: NAME_PATTERNS is a module constant.

        plan_package calls this on entry, so without the cache the patterns
        recompile once per package -- 30-odd times per full run, plus again in
        the backlink audit -- on the walk cli.py bothers to time. The result
        cannot differ between calls, and IgnoreRules is frozen, so sharing one
        instance is safe.
        """
        return cls(names=tuple(re.compile(p) for p in NAME_PATTERNS))

    def matches(self, rel: Path) -> bool:
        """True if any component of a package-relative path is ignored."""
        return any(
            pattern.fullmatch(part) for pattern in self.names for part in rel.parts
        )
