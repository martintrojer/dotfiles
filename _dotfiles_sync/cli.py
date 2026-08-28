from __future__ import annotations

import argparse
import logging
import os
from collections.abc import Callable
from datetime import datetime
from pathlib import Path
from time import monotonic

from .config import BACKUP_DIR_NAME, task_enabled
from .external import (
    apply_tmux_tpm,
    apply_zsh_plugins,
)
from .fedora_systemd import apply_fedora_systemd_masks, check_fedora_systemd_masks
from .integration_checks import (
    check_murmur,
    check_tmux_tpm,
    check_zsh_plugins,
)
from .inventory import build_specs, group_active_packages, resolve_requested_packages
from .model import Args, Overwrite, PackageSpec, TaskPolicy
from .repo_checks import (
    check_package_coverage,
    check_private_env_mistakes,
    check_repo_backlinks,
    check_systemd_unit_targets,
    prune_managed_ignored_artifact_links,
    prune_stale_managed_links,
)
from .sync import run_apply_group, run_check_group
from .system import active_scopes, detect_system

LOGGER = logging.getLogger("dotfiles-sync")


def parse_args() -> Args:
    parser = argparse.ArgumentParser(
        description="Manage all relevant dotfile packages in this repository."
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "-c",
        "--check",
        dest="action",
        action="store_const",
        const="check",
        help="Check everything (or selected packages) for missing links and conflicts (default)",
    )
    mode.add_argument(
        "-a",
        "--apply",
        dest="action",
        action="store_const",
        const="apply",
        help="Apply everything (or just the selected packages)",
    )
    parser.set_defaults(action="check")
    parser.add_argument(
        "-f",
        "--force-overwrite",
        action="store_true",
        help=(
            "Move conflicting target paths into a timestamped backup dir under "
            "the target before stowing"
        ),
    )
    parser.add_argument(
        "--show-diffs",
        action="store_true",
        help="In check mode, show diff commands for plain-file conflicts",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Show verbose stow output and detailed custom checks",
    )
    parser.add_argument(
        "-t",
        "--target",
        default=os.environ.get("TARGET", str(Path.home())),
        help="Override target directory (default: %(default)s)",
    )
    parser.add_argument(
        "--ignore",
        action="append",
        default=[],
        metavar="ID",
        help="Suppress a specific check failure by ID (repeatable)",
    )
    parser.add_argument(
        "--skip-gaming",
        action="store_true",
        help=(
            "Drop the Fedora gaming layer (fedora/gaming/). Use on work/laptop "
            "hosts that want a pure Sway baseline with no gaming footprint. "
            "On by default; the main gaming rig needs no flag."
        ),
    )
    parser.add_argument(
        "packages",
        nargs="*",
        metavar="PACKAGE",
        help=(
            "Only process these stow packages (for example: ./dotfiles-sync "
            "--apply nvim tmux)"
        ),
    )
    namespace = parser.parse_args()
    return Args(
        action=namespace.action,
        force_overwrite=namespace.force_overwrite,
        show_diffs=namespace.show_diffs,
        verbose=namespace.verbose,
        target=namespace.target,
        ignore=set(namespace.ignore),
        packages=tuple(namespace.packages),
        skip_gaming=namespace.skip_gaming,
    )


def configure_logging(args: Args) -> None:
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.WARNING,
        format="%(message)s",
    )


def run_check_tasks(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    full_run: bool,
    verbose: bool,
    ignore: set[str],
) -> bool:
    def _check_repo_backlinks() -> bool:
        LOGGER.info("\n[repo-backlinks]")
        LOGGER.info("Scanning for stale or invalid repo backlinks...")
        backlink_scan_started = monotonic()
        has_issues = check_repo_backlinks(target, specs, active_names, ignore=ignore)
        backlink_scan_elapsed = monotonic() - backlink_scan_started
        LOGGER.info(f"Backlink scan finished in {backlink_scan_elapsed:.1f}s.")
        return has_issues

    # (policy, handler) pairs, not a name-keyed dict beside a separate name
    # list: the two cannot drift apart, so there is no lookup to fail.
    tasks: tuple[tuple[TaskPolicy, Callable[[], bool]], ...] = (
        (
            TaskPolicy("package-coverage", full_run_only=True),
            lambda: check_package_coverage(specs, ignore=ignore),
        ),
        (
            TaskPolicy("private-env"),
            lambda: check_private_env_mistakes(ignore=ignore),
        ),
        (
            TaskPolicy("zsh-plugins", packages=frozenset({"zsh"})),
            lambda: check_zsh_plugins(target, verbose=verbose, ignore=ignore),
        ),
        (
            TaskPolicy("tmux-tpm", packages=frozenset({"tmux"})),
            lambda: check_tmux_tpm(target, verbose=verbose, ignore=ignore),
        ),
        # Both packages depend on it: tmux shells out to murmur for the status
        # segment, the picker and the focus hooks; pi hosts its extension.
        (
            TaskPolicy("murmur", packages=frozenset({"tmux", "pi"})),
            lambda: check_murmur(target, verbose=verbose, ignore=ignore),
        ),
        (
            TaskPolicy("fedora-systemd-masks", packages=frozenset({"systemd"})),
            lambda: check_fedora_systemd_masks(target, verbose=verbose, ignore=ignore),
        ),
        # Static, filesystem-only: no systemd, no stowed target, so it runs
        # everywhere and on every run rather than only on Fedora.
        (
            TaskPolicy("systemd-unit-targets"),
            lambda: check_systemd_unit_targets(specs, ignore=ignore),
        ),
        (
            TaskPolicy("repo-backlinks", full_run_only=True),
            _check_repo_backlinks,
        ),
    )

    has_issues = False
    for task, handler in tasks:
        if not task_enabled(task, active_names=active_names, full_run=full_run):
            continue
        has_issues |= handler()
    return has_issues


def run_apply_tasks(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    full_run: bool,
    verbose: bool,
) -> None:
    # Pairs, not a name join. A KeyError here would land mid---apply, after
    # earlier tasks had already cloned repos and written symlinks.
    tasks: tuple[tuple[TaskPolicy, Callable[[], None]], ...] = (
        (
            TaskPolicy("ignored-artifacts"),
            lambda: prune_managed_ignored_artifact_links(
                target,
                specs,
                active_names,
                verbose=verbose,
            ),
        ),
        # Full-run only, matching the repo-backlinks check that reports these.
        # A package-scoped run scans just that package, so it cannot tell a
        # stale link from one belonging to a package it was not asked about.
        (
            TaskPolicy("stale-symlinks", full_run_only=True),
            lambda: prune_stale_managed_links(target, specs, active_names),
        ),
        (
            TaskPolicy("fedora-systemd-masks", packages=frozenset({"systemd"})),
            lambda: apply_fedora_systemd_masks(target, verbose=verbose),
        ),
        (
            TaskPolicy("zsh-plugins", packages=frozenset({"zsh"})),
            lambda: apply_zsh_plugins(target, verbose=verbose),
        ),
        (
            TaskPolicy("tmux-tpm", packages=frozenset({"tmux"})),
            lambda: apply_tmux_tpm(target, verbose=verbose),
        ),
    )

    for task, handler in tasks:
        if not task_enabled(task, active_names=active_names, full_run=full_run):
            continue
        handler()


def main() -> int:
    args = parse_args()
    configure_logging(args)

    target = Path(args.target).expanduser()
    if not target.is_dir():
        LOGGER.error(f"Target directory does not exist: {target}")
        return 1

    system = detect_system()
    specs = build_specs()
    scopes = active_scopes(system, skip_gaming=args.skip_gaming)
    active_names = resolve_requested_packages(specs, args.packages, scopes)
    full_run = not args.packages
    overwrite = None
    if args.force_overwrite:
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        overwrite = Overwrite(backup_root=target / BACKUP_DIR_NAME / timestamp)

    LOGGER.info(f"Using target: {target}")
    LOGGER.info(f"Detected OS: {system.os_name}")
    LOGGER.info(f"Mode: {args.action}")
    if overwrite is not None:
        LOGGER.info(
            "Force overwrite enabled; conflicting targets will be moved to: "
            f"{overwrite.backup_root}"
        )

    groups = group_active_packages(specs, active_names)

    has_issues = False
    for label, packages in groups.items():
        if args.action == "check":
            has_issues |= run_check_group(
                label,
                packages,
                specs,
                target,
                args.show_diffs,
                args.verbose,
                ignore=args.ignore,
            )
        else:
            run_apply_group(
                label,
                packages,
                specs,
                target,
                verbose=args.verbose,
                overwrite=overwrite,
                ignore=args.ignore,
            )

    if args.action == "check":
        has_issues |= run_check_tasks(
            target,
            specs,
            active_names,
            full_run=full_run,
            verbose=args.verbose,
            ignore=args.ignore,
        )
        if has_issues:
            print("\nIssues found.")
            return 1
        print("No missing links or conflicts found.")
        return 0

    run_apply_tasks(
        target,
        specs,
        active_names,
        full_run=full_run,
        verbose=args.verbose,
    )
    print("\nDone.")
    if full_run:
        print_post_apply_hints()
    return 0


def print_post_apply_hints() -> None:
    print()
    print(
        "Skills and pi extensions are now symlinked at "
        "~/.agents/skills/ and ~/.pi/agent/extensions/."
    )
    print(
        "Codex / OpenCode / Pi / Cursor / OpenClaw / etc. read those "
        "paths automatically."
    )
    print()
