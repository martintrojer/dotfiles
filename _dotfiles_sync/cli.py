from __future__ import annotations

import argparse
import logging
import os
from collections.abc import Callable
from datetime import datetime
from pathlib import Path
from time import monotonic

from .config import APPLY_TASKS, BACKUP_DIR_NAME, CHECK_TASKS, task_enabled
from .external import (
    apply_pi_extensions_symlinks,
    apply_skills_symlinks,
    apply_tmux_tpm,
    apply_zsh_plugins,
)
from .integration_checks import (
    check_agent_notify,
    check_tmux_tpm,
    check_zsh_plugins,
)
from .inventory import build_specs, group_active_packages, resolve_requested_packages
from .model import Args, PackageSpec
from .pins import GITHUB_SLUG
from .repo_checks import (
    check_package_coverage,
    check_private_env_mistakes,
    check_repo_backlinks,
    prune_managed_ignored_artifact_links,
)
from .stow import ensure_stow_available, run_apply_group, run_check_group
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
        help="Check everything (or selected packages) for missing stows and conflicts (default)",
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

    handlers: dict[str, Callable[[], bool]] = {
        "package-coverage": lambda: check_package_coverage(specs, ignore=ignore),
        "private-env": lambda: check_private_env_mistakes(ignore=ignore),
        "zsh-plugins": lambda: check_zsh_plugins(
            target, verbose=verbose, ignore=ignore
        ),
        "tmux-tpm": lambda: check_tmux_tpm(target, verbose=verbose, ignore=ignore),
        "agent-notify": lambda: check_agent_notify(
            target, verbose=verbose, ignore=ignore
        ),
        "repo-backlinks": _check_repo_backlinks,
    }

    has_issues = False
    for task in CHECK_TASKS:
        if not task_enabled(task, active_names=active_names, full_run=full_run):
            continue
        has_issues |= handlers[task.name]()
    return has_issues


def run_apply_tasks(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    full_run: bool,
    verbose: bool,
) -> None:
    handlers: dict[str, Callable[[], None]] = {
        "ignored-artifacts": lambda: prune_managed_ignored_artifact_links(
            target,
            specs,
            active_names,
            verbose=verbose,
        ),
        "zsh-plugins": lambda: apply_zsh_plugins(target, verbose=verbose),
        "tmux-tpm": lambda: apply_tmux_tpm(target, verbose=verbose),
        "agents-skills": lambda: apply_skills_symlinks(target, verbose=verbose),
        "pi-extensions": lambda: apply_pi_extensions_symlinks(target, verbose=verbose),
    }

    for task in APPLY_TASKS:
        if not task_enabled(task, active_names=active_names, full_run=full_run):
            continue
        handlers[task.name]()


def main() -> int:
    args = parse_args()
    ensure_stow_available()
    configure_logging(args)

    target = Path(args.target).expanduser()
    if not target.is_dir():
        LOGGER.error(f"Target directory does not exist: {target}")
        return 1

    system = detect_system()
    specs = build_specs()
    scopes = active_scopes(system)
    active_names = resolve_requested_packages(specs, args.packages, scopes)
    full_run = not args.packages
    backup_root = None
    if args.force_overwrite:
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        backup_root = target / BACKUP_DIR_NAME / timestamp

    LOGGER.info(f"Using target: {target}")
    LOGGER.info(f"Detected OS: {system.os_name}")
    LOGGER.info(f"Mode: {args.action}")
    if backup_root is not None:
        LOGGER.info(
            "Force overwrite enabled; conflicting targets will be moved to: "
            f"{backup_root}"
        )

    groups = group_active_packages(specs, active_names)

    has_issues = False
    for (label, stow_dir), packages in groups.items():
        if args.action == "check":
            has_issues |= run_check_group(
                label,
                stow_dir,
                packages,
                target,
                args.show_diffs,
                args.verbose,
                ignore=args.ignore,
            )
        else:
            run_apply_group(
                label,
                stow_dir,
                packages,
                target,
                verbose=args.verbose,
                force_overwrite=args.force_overwrite,
                backup_root=backup_root,
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
        print("No missing stows or conflicts found.")
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
    print("Two manual steps remain:")
    print()
    print("  # 1. Claude Code plugin (one-time marketplace add, then")
    print("  #    re-run `plugin install` after every push to refresh):")
    print(f"  claude plugin marketplace add {GITHUB_SLUG}")
    print("  claude plugin install mtrojer@dotfiles")
    print()
    print("  # 2. Codex notify hook — add this line to ~/.codex/config.toml:")
    print(
        '  notify = ["/bin/sh", "-lc", "python3 \\"$HOME/.config/tmux/scripts/'
        'agent-attention\\" notify --source codex --event-type notify --title Codex"]'
    )
    print()
