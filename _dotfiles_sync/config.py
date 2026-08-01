from __future__ import annotations

import logging
from pathlib import Path
from typing import Final

from .model import TaskPolicy

SCRIPT_DIR: Final[Path] = Path(__file__).resolve().parent.parent

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")
BACKUP_DIR_NAME: Final[str] = ".dotfiles-sync-backups"

# The task tables live in cli.py, paired with their handlers. Keeping the
# policy here and the callable there joined them by a name string that
# nothing checked, so a rename raised KeyError partway through --apply,
# after earlier tasks had already mutated the target tree. Moving the
# callables here instead would make config.py import external/
# fedora_systemd/integration_checks, inverting the dependency direction.


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
