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
    # Most packages link per-leaf: every file gets its own symlink and parent
    # directories are real, so two packages can contribute entries to one
    # target dir (local-bin and fedora/bin both fill ~/.local/bin).
    #
    # bundle_dirs names package-relative directories whose *children* link as
    # opaque directory symlinks instead. skills/ sets `.agents/skills` so each
    # skill lands as one bundle and its vendored README/LICENSE ride along
    # past the ignore rules, which is what stow's folding used to do -- except
    # the depth is stated here rather than coaxed out of it with anchor mkdirs.
    bundle_dirs: tuple[Path, ...] = ()

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
