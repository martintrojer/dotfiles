from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
from typing import Literal, TypeAlias

Action: TypeAlias = Literal["check", "apply"]
PackageScope: TypeAlias = Literal["common", "darwin", "linux", "fedora"]
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


@dataclass(frozen=True)
class PackageSpec:
    name: str
    stow_dir: Path
    scope: PackageScope

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
