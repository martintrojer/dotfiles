"""Check and apply a package's symlinks: policy over the planner.

link.py decides what links a package wants and what state each one is in.
This module decides what to do about it -- report conflicts, honour --ignore,
back up under --force-overwrite, and skip a package rather than half-link it.
"""

from __future__ import annotations

import filecmp
import logging
import shlex
import shutil
from pathlib import Path

from .link import Link, LinkState, apply_link, plan_package
from .model import PackageSpec

LOGGER = logging.getLogger("dotfiles-sync")


def plan_group(
    packages: list[str], specs: dict[str, PackageSpec], target: Path
) -> list[Link]:
    links: list[Link] = []
    for name in packages:
        spec = specs[name]
        links.extend(
            plan_package(
                name,
                spec.package_dir,
                target,
                bundle_dirs=spec.bundle_dirs,
            )
        )
    return links


def shlex_quote(path: Path) -> str:
    return shlex.quote(str(path))


def show_conflict_diffs(conflicts: list[Link]) -> None:
    found = False
    for link in conflicts:
        if not link.source.is_file() or not link.target.is_file():
            continue
        if not found:
            LOGGER.warning("Diffs:")
            found = True
        if filecmp.cmp(link.target, link.source, shallow=False):
            LOGGER.warning(
                f"  {link.rel} matches repo copy; "
                "safe to replace with --apply --force-overwrite"
            )
        else:
            LOGGER.warning(
                f"  diff -u {shlex_quote(link.target)} {shlex_quote(link.source)}"
            )


def backup_conflict_path(link: Link, backup_root: Path) -> None:
    if not link.target.exists() and not link.target.is_symlink():
        return
    backup_path = backup_root / link.rel
    backup_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.move(str(link.target), str(backup_path))
    LOGGER.warning(f"BACKED UP: {link.target} -> {backup_path}")


def run_check_group(
    label: str,
    packages: list[str],
    specs: dict[str, PackageSpec],
    target: Path,
    show_diffs: bool,
    verbose: bool,
    *,
    ignore: set[str],
) -> bool:
    """Report what --apply would do. True if anything needs attention."""
    if not packages:
        return False

    links = plan_group(packages, specs, target)
    conflicts = [
        link
        for link in links
        if link.state is LinkState.CONFLICT and f"conflict:{link.rel}" not in ignore
    ]
    pending = [link for link in links if link.is_actionable]

    if not conflicts and not pending:
        if verbose:
            LOGGER.debug(f"\n[{label}]")
            LOGGER.debug("OK")
        return False

    LOGGER.warning(f"\n[{label}]")
    for link in pending:
        verb = "LINK" if link.state is LinkState.MISSING else "RELINK"
        LOGGER.warning(f"  {verb}: {link.rel} -> {link.source}")
    for link in conflicts:
        LOGGER.warning(f"  CONFLICT: {link.rel} (exists, not ours)")
        LOGGER.warning(f"  (--ignore conflict:{link.rel})")
    if show_diffs:
        show_conflict_diffs(conflicts)

    # Pending links alone are not a failure in --check: that is just drift the
    # next --apply fixes. Only conflicts need a human.
    return bool(conflicts)


def run_apply_group(
    label: str,
    packages: list[str],
    specs: dict[str, PackageSpec],
    target: Path,
    verbose: bool,
    force_overwrite: bool,
    backup_root: Path | None,
    *,
    ignore: set[str],
) -> None:
    if not packages:
        return

    # Drop ignored conflicts from the plan entirely, not just from `conflicts`:
    # --ignore conflict:<rel> means "leave that target to me", and anything
    # still in `links` gets unlinked -- unbacked -- under --force-overwrite.
    links = [
        link
        for link in plan_group(packages, specs, target)
        if not (link.state is LinkState.CONFLICT and f"conflict:{link.rel}" in ignore)
    ]
    conflicts = [link for link in links if link.state is LinkState.CONFLICT]

    if conflicts and not force_overwrite:
        # Skip whole packages that have an unresolved conflict rather than
        # half-linking them, matching the previous behaviour.
        blocked = {link.package for link in conflicts}
        for package in sorted(blocked):
            LOGGER.warning(f"Skipping package '{package}' (conflict; see --check)")
        links = [link for link in links if link.package not in blocked]

    if force_overwrite and conflicts:
        assert backup_root is not None
        LOGGER.warning(f"\n[{label}]")
        for link in conflicts:
            backup_conflict_path(link, backup_root)

    for link in links:
        if link.state is LinkState.OK:
            continue
        if link.state is LinkState.CONFLICT and not force_overwrite:
            continue
        if link.state is LinkState.STALE:
            LOGGER.warning(f"CLEARED STALE: {link.target}")
        apply_link(link)
        if verbose:
            LOGGER.debug(f"LINK: {link.rel} -> {link.source}")
