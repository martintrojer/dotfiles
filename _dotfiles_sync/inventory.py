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
            "pi",
            "skills",
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
            "foot",
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
    # Gaming layer: the main rig's opt-out module. Active by default on Fedora,
    # suppressed with `--skip-gaming` on work/laptop hosts so they get a pure
    # Sway baseline with zero gaming footprint. The single "home" stow package
    # holds every gaming dotfile (bin helpers, MangoHud/GameMode configs, the
    # Sunshine unit override); the rest of fedora/gaming/ is non-stow setup
    # scripts, data, and docs. See fedora/gaming/README.md.
    (
        "gaming",
        ROOT / "fedora" / "gaming",
        ("home",),
    ),
)

# Top-level directories that exist on disk but are not stow packages and should
# not trip the package-coverage check. Dot-prefixed dirs are auto-skipped, so
# .git/.jj/.ruff_cache/.vecgrep don't need to be listed here.
IGNORED_TOPLEVEL_DIRS: Final[set[str]] = {
    "__pycache__",
    "_dotfiles_sync",
    "docs",  # cross-cutting documentation hosted at repo root
    "fedora",  # contains its own stow packages, scope-driven
    "guides",  # interactive learning guides, rendered by guides/build.py
}

# Packages that stow with folding instead of the global --no-folding, mapping
# each to the fold-anchor dirs that must exist before stowing so the fold lands
# at the right level. See PackageSpec.fold / .fold_anchors.
FOLDED_PACKAGES: Final[dict[str, tuple[Path, ...]]] = {
    # skills/<name>/ must link as one opaque directory symlink so each skill's
    # vendored README/LICENSE ride along past the .stowrc ignore rules.
    "skills": (Path(".agents") / "skills",),
}


def build_specs() -> dict[str, PackageSpec]:
    return {
        name: PackageSpec(
            name=name,
            stow_dir=stow_dir,
            scope=scope,
            fold=name in FOLDED_PACKAGES,
            fold_anchors=FOLDED_PACKAGES.get(name, ()),
        )
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
) -> dict[tuple[PackageScope, Path, bool], list[str]]:
    # Folding differs per package and is a stow-invocation flag, so it joins
    # scope+stow_dir in the group key: folded packages get their own stow run.
    groups: dict[tuple[PackageScope, Path, bool], list[str]] = {}
    for name in sorted(active_names):
        spec = specs[name]
        key = (spec.scope, spec.stow_dir, spec.fold)
        groups.setdefault(key, []).append(name)
    return groups
