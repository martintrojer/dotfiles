from __future__ import annotations

import logging
from pathlib import Path
from typing import Final

from .model import PackageScope, PackageSpec

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")
ROOT: Final[Path] = Path(__file__).resolve().parent.parent

# (scope, stow_dir, package_names)
PACKAGE_GROUPS: Final[tuple[tuple[PackageScope, Path, tuple[str, ...]], ...]] = (
    (
        "common",
        ROOT,
        (
            "bat",
            "btop",
            "eza",
            "gdu",
            "git",
            "glow",
            "jj",
            "local-bin",
            "nvim",
            "opencode",
            "ssh",
            "summarize",
            "tmux",
            "tuicr",
            "vale",
            "yazi",
            "zsh",
        ),
    ),
    (
        "darwin",
        ROOT,
        ("ghostty", "hammerspoon"),
    ),
    (
        "linux",
        ROOT,
        (
            "alacritty",
            "fuzzel",
            "kanshi",
            "mako",
            "sway",
            "swaylock",
            "waybar",
        ),
    ),
    (
        "fedora",
        ROOT / "fedora",
        (
            "bin",
            "containers",
            "gtk-3.0",
            "systemd",
        ),
    ),
)

# Top-level directories that exist on disk but are not stow packages and should
# not trip the package-coverage check. Dot-prefixed dirs are auto-skipped, so
# .git/.jj/.claude-plugin/.ruff_cache/.vecgrep don't need to be listed here.
IGNORED_TOPLEVEL_DIRS: Final[set[str]] = {
    "__pycache__",
    "_dotfiles_sync",
    "agents",  # Claude plugin asset, consumed via `claude plugin install`
    "docs",  # cross-cutting documentation hosted at repo root
    "fedora",  # contains its own stow packages, scope-driven
    "hooks",  # Claude plugin asset, consumed via `claude plugin install`
    "pi",  # source for pi/extensions/, symlinked by --apply
    "skills",  # source for shared skills, symlinked by --apply
}


def build_specs() -> dict[str, PackageSpec]:
    return {
        name: PackageSpec(name=name, stow_dir=stow_dir, scope=scope)
        for scope, stow_dir, names in PACKAGE_GROUPS
        for name in names
    }


def resolve_requested_packages(
    specs: dict[str, PackageSpec],
    requested: tuple[str, ...],
    scopes: set[PackageScope],
) -> set[str]:
    if not requested:
        return {name for name, spec in specs.items() if spec.scope in scopes}

    unknown = sorted({name for name in requested if name not in specs})
    if unknown:
        available = ", ".join(sorted(specs))
        LOGGER.error(f"Unknown package(s): {', '.join(unknown)}")
        LOGGER.error(f"Available packages: {available}")
        raise SystemExit(2)

    unsupported = sorted(
        {name for name in requested if specs[name].scope not in scopes}
    )
    if unsupported:
        details = ", ".join(f"{name}[{specs[name].scope}]" for name in unsupported)
        LOGGER.error(f"Requested package(s) are not active on this machine: {details}")
        raise SystemExit(2)

    return set(requested)


def group_active_packages(
    specs: dict[str, PackageSpec], active_names: set[str]
) -> dict[tuple[PackageScope, Path], list[str]]:
    groups: dict[tuple[PackageScope, Path], list[str]] = {}
    for name in sorted(active_names):
        spec = specs[name]
        key = (spec.scope, spec.stow_dir)
        groups.setdefault(key, []).append(name)
    return groups
