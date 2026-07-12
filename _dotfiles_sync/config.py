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
