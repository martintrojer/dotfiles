from __future__ import annotations

import logging
from collections.abc import Callable
from pathlib import Path
from typing import Final

from .model import TaskPolicy

REPO_ROOT: Final[Path] = Path(__file__).resolve().parent.parent

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")
BACKUP_DIR_NAME: Final[str] = ".dotfiles-sync-backups"

# Directories (and a few repo-relative paths) that repo walks never descend
# into. Shared by the git-less fallback in repo_checks.iter_repo_files and
# the palette-hex audit in render_theme. Prefer .gitignore for committable
# scans; this list is the fallback, and the audit's only skip set.
REPO_WALK_SKIP_DIRS: Final[frozenset[str]] = frozenset(
    {
        ".direnv",
        ".git",
        ".jj",
        ".mypy_cache",
        ".nox",
        ".pytest_cache",
        ".ruff_cache",
        ".tox",
        ".venv",
        "__pycache__",
        "build",
        "dist",
        "guides/build",
        "node_modules",
    }
)

# The task tables live in cli.py, paired with their handlers. Keeping the
# policy here and the callable there joined them by a name string that
# nothing checked, so a rename raised KeyError partway through --apply,
# after earlier tasks had already mutated the target tree. Moving the
# callables here instead would make config.py import external/
# fedora_systemd/integration_checks, inverting the dependency direction.


def lazy_header(name: str) -> Callable[[], None]:
    """Return a callable that logs ``[name]`` once, on the first call.

    Sections only announce themselves if they have something to report, so a
    clean run stays quiet. That latch was hand-rolled as an identical six-line
    closure in three places and open-coded as ``if not found_issue:`` in a
    fourth, where it shared a flag with the return value -- so adding a branch
    meant remembering to print the header in it.

    The leading newline is part of the format: it separates sections in the
    output, so the first thing printed is a blank line, not a bare header.
    """
    printed = False

    def print_header() -> None:
        nonlocal printed
        if not printed:
            LOGGER.warning(f"\n[{name}]")
            printed = True

    return print_header


def task_enabled(
    task: TaskPolicy,
    *,
    active_names: set[str],
    full_run: bool,
) -> bool:
    if task.full_run_only:
        return full_run
    if not task.packages:
        return True
    return bool(task.packages & active_names)
