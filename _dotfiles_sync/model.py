from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Literal, TypeAlias

Action: TypeAlias = Literal["check", "apply"]
PackageScope: TypeAlias = Literal["common", "darwin", "linux", "fedora", "gaming"]
Conflict: TypeAlias = tuple[str, str]


@dataclass(frozen=True)
class Args:
    action: Action
    force_overwrite: bool
    show_diffs: bool
    verbose: bool
    target: str
    ignore: set[str]
    packages: tuple[str, ...]
    skip_gaming: bool


@dataclass(frozen=True)
class PackageSpec:
    name: str
    stow_dir: Path
    scope: PackageScope
    # Most packages stow with --no-folding (per-leaf symlinks) to avoid the
    # "two scopes share a target dir" folding bug. A few packages instead want
    # stow's default folding so a whole source subtree becomes one directory
    # symlink (e.g. skills/, where each skill must link as an opaque bundle so
    # vendored README/LICENSE files ride along past the .stowrc ignore rules).
    fold: bool = False
    # Target dirs to mkdir before stowing. For folded packages this pins the
    # fold level: without a pre-existing real dir, stow folds one level too
    # high (e.g. ~/.agents instead of ~/.agents/skills/<name>).
    fold_anchors: tuple[Path, ...] = ()

    @property
    def package_dir(self) -> Path:
        return self.stow_dir / self.name


@dataclass(frozen=True)
class SystemInfo:
    os_name: str
    is_fedora: bool


@dataclass(frozen=True)
class TaskPolicy:
    name: str
    packages: frozenset[str] = frozenset()
    full_run_only: bool = False
