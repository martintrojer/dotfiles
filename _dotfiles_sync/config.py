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

# stow emits this for a pre-existing symlink it did not create. When such a link
# points back into this repo at a path that no longer exists (the usual cause:
# an intra-repo file move), it is a stale managed link that --force-overwrite
# should clear. Links to anything outside the repo are left alone.
FOREIGN_TARGET_RE: Final[re.Pattern[str]] = re.compile(
    r"^  \* existing target is not owned by stow: (.*)$"
)

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
