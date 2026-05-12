from __future__ import annotations

import logging
import re
from pathlib import Path
from typing import Final

from .model import TaskPolicy

SCRIPT_DIR: Final[Path] = Path(__file__).resolve().parent.parent
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
    TaskPolicy("repo-backlinks", full_run_only=True),
)

APPLY_TASKS: Final[tuple[TaskPolicy, ...]] = (
    TaskPolicy("ignored-artifacts"),
    TaskPolicy("zsh-plugins", packages=frozenset({"zsh"})),
    TaskPolicy("tmux-tpm", packages=frozenset({"tmux"})),
    TaskPolicy("agents-skills", full_run_only=True),
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
