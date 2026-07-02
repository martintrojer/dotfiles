from __future__ import annotations

import logging
import os
import re
from pathlib import Path
from typing import Final

from .model import TaskPolicy

SCRIPT_DIR: Final[Path] = Path(__file__).resolve().parent.parent


def relative_symlink_target(source: Path, dest: Path) -> str:
    """Relative target for a symlink at `dest` pointing to `source`.

    Resolves `dest`'s parent first so the `..` count is correct even when
    HOME contains symlinked components (e.g. /home -> var/home on Fedora
    Silverblue), since the kernel resolves `..` physically, not lexically.
    """
    return os.path.relpath(source, dest.parent.resolve())


LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")
BACKUP_DIR_NAME: Final[str] = ".dotfiles-sync-backups"

CONFLICT_RE: Final[re.Pattern[str]] = re.compile(
    r"^  \* cannot stow (.*) over existing target (.*) since .*"
)

CHECK_TASKS: Final[tuple[TaskPolicy, ...]] = (
    TaskPolicy("package-coverage", full_run_only=True),
    TaskPolicy("private-env"),
    TaskPolicy("zsh-plugins", packages=frozenset({"zsh"})),
    TaskPolicy("tmux-tpm", packages=frozenset({"tmux"})),
    TaskPolicy("agent-notify", full_run_only=True),
    TaskPolicy("fedora-systemd-masks", packages=frozenset({"systemd"})),
    TaskPolicy("repo-backlinks", full_run_only=True),
)

APPLY_TASKS: Final[tuple[TaskPolicy, ...]] = (
    TaskPolicy("ignored-artifacts"),
    TaskPolicy("fedora-systemd-masks", packages=frozenset({"systemd"})),
    TaskPolicy("zsh-plugins", packages=frozenset({"zsh"})),
    TaskPolicy("tmux-tpm", packages=frozenset({"tmux"})),
    TaskPolicy("skills", full_run_only=True),
    TaskPolicy("pi-extensions", full_run_only=True),
)


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
