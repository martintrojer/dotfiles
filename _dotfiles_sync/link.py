"""Symlink planner and applier: the job GNU Stow used to do for us.

Stow was doing very little here. We ran it with ``--no-folding`` on 33 of 34
packages, which disables its signature feature, then regex-scraped its English
output to recover the package/source/target structure we had handed it in the
first place. Owning the walk turns conflicts into objects, collapses two
implementations of the ignore rules into one, and replaces the fold-anchor
mkdir dance with an explicit per-package flag.

The layout contract is unchanged: a package directory mirrors ``$HOME``, so
``zsh/.zshrc`` links to ``~/.zshrc``. Two link modes:

* per-leaf (default) -- every file becomes its own symlink and intermediate
  directories are created for real. Predictable, and ``ls -la ~/.config/foo``
  always shows where each entry points. Stow's folding caused a bug class
  where two scopes contributing to one target dir (``local-bin`` and
  ``fedora/bin`` both writing ``~/.local/bin``) fought over it.
* bundle (``PackageSpec.bundle_dirs``) -- children of a named directory link
  as opaque directory symlinks. ``skills/`` sets ``.agents/skills`` so each
  skill links as one bundle and its vendored README/LICENSE ride along past
  the ignore rules.
"""

from __future__ import annotations

import logging
import os
from collections.abc import Iterator
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

from .config import SCRIPT_DIR
from .ignore import IgnoreRules

LOGGER = logging.getLogger("dotfiles-sync")


class LinkState(Enum):
    """What the target path looks like relative to the link we want."""

    OK = "ok"  # already the symlink we want
    MISSING = "missing"  # nothing there; safe to create
    STALE = "stale"  # our symlink, but pointing at a path that no longer exists
    CONFLICT = "conflict"  # occupied by something we do not own


@dataclass(frozen=True)
class Link:
    """One planned symlink: `source` in the repo, `target` under $HOME."""

    package: str
    source: Path  # absolute path in the repo
    target: Path  # absolute path under the target root
    state: LinkState
    rel: str  # target path relative to the target root, for messages

    @property
    def is_actionable(self) -> bool:
        return self.state in (LinkState.MISSING, LinkState.STALE)


def _classify(source: Path, target: Path, repo_root: Path) -> LinkState:
    if not target.is_symlink():
        return LinkState.MISSING if not target.exists() else LinkState.CONFLICT

    raw = Path(os.readlink(target))
    resolved = (target.parent / raw).resolve(strict=False)
    if resolved == source.resolve():
        return LinkState.OK

    # A symlink of ours whose destination has moved or been deleted. Distinct
    # from CONFLICT because --force-overwrite may clear it without a backup:
    # there is nothing left to lose. A dangling link pointing *outside* the
    # repo belongs to someone else and stays a conflict.
    if resolved.is_relative_to(repo_root) and not resolved.exists():
        return LinkState.STALE

    return LinkState.CONFLICT


def _iter_sources(
    package_dir: Path, *, bundle_dirs: tuple[Path, ...], ignore: IgnoreRules
) -> Iterator[Path]:
    """Yield repo paths that should become symlinks.

    Per-leaf by default: descend and yield files (and any symlink, even to a
    directory, since the repo may vendor one). Children of a bundle_dir are
    yielded as directories instead and not descended into.
    """
    bundles = {package_dir / d for d in bundle_dirs}
    for bundle in sorted(bundles):
        if not bundle.is_dir():
            continue
        for entry in sorted(bundle.iterdir()):
            if ignore.matches(entry.relative_to(package_dir)):
                continue
            yield entry

    for entry in sorted(package_dir.rglob("*")):
        rel = entry.relative_to(package_dir)
        if ignore.matches(rel):
            continue
        # Anything at or below a bundle dir is already covered above.
        if any(entry == b or b in entry.parents for b in bundles):
            continue
        if entry.is_dir() and not entry.is_symlink():
            continue
        yield entry


def plan_package(
    name: str,
    package_dir: Path,
    target_root: Path,
    *,
    bundle_dirs: tuple[Path, ...],
    repo_root: Path | None = None,
) -> list[Link]:
    """Compute every link this package wants, with its current state.

    `repo_root` bounds what counts as "a link of ours" for STALE detection;
    it defaults to the real repo and is overridden by tests.
    """
    root = (repo_root or SCRIPT_DIR).resolve()
    ignore = IgnoreRules.load()
    links: list[Link] = []
    for source in _iter_sources(package_dir, bundle_dirs=bundle_dirs, ignore=ignore):
        rel = source.relative_to(package_dir)
        target = target_root / rel
        links.append(
            Link(
                package=name,
                source=source,
                target=target,
                state=_classify(source, target, root),
                rel=str(rel),
            )
        )
    return links


def apply_link(link: Link) -> None:
    """Create one symlink, replacing a stale one and making parents as needed.

    Parents are real directories, never symlinks: that is what keeps two
    packages able to contribute entries to the same target directory.

    A real directory at the target is refused rather than removed. This is the
    one function here that deletes user data, and a directory means either the
    caller skipped the conflict/backup policy in sync.py or two packages in one
    group disagree about whether a path is a file or a directory -- the plan is
    computed before any of it is applied, so an earlier link's mkdir can turn a
    path another link planned as MISSING into a directory. Deleting a tree to
    resolve that is never the right guess; unlink() would raise
    IsADirectoryError here, which says nothing about what to do next.
    """
    link.target.parent.mkdir(parents=True, exist_ok=True)
    if link.target.is_dir() and not link.target.is_symlink():
        raise SystemExit(
            f"dotfiles-sync: refusing to replace directory {link.target}\n"
            f"  wanted a symlink to {link.source} (package '{link.package}')\n"
            "  move or remove the directory yourself, then re-run"
        )
    if link.target.is_symlink() or link.target.exists():
        link.target.unlink()
    link.target.symlink_to(link.source)
