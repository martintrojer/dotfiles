"""Ensure ~/.cursor/hooks.json includes our temporary OSC 7501 bridge hook.

Temporary bridge until Cursor Agent ships native OSC 7501 support (Claude Code,
Pi, and Codex already emit it natively). Watch upstream and remove this module
plus cursor/.cursor/hooks/osc7501 once native support lands.

~/.cursor/hooks.json is shared with other tools (murmur, mu, muzzle, museum),
so it is not linked. --check reports missing entries and --apply appends only
the missing ones, leaving every other event and hook intact.
"""

from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Any, Final

from .config import lazy_header

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")
ISSUE_ID: Final[str] = "osc7501-hooks"

CURSOR_HOOKS_PATH: Final[Path] = Path(".cursor") / "hooks.json"
CURSOR_COMMAND: Final[str] = "$HOME/.cursor/hooks/osc7501"

CURSOR_EVENTS: Final[tuple[str, ...]] = (
    "sessionStart",
    "beforeSubmitPrompt",
    "beforeShellExecution",
    "stop",
    "sessionEnd",
)


def _has_command(entries: list[Any], command: str) -> bool:
    return any(
        isinstance(entry, dict) and entry.get("command") == command for entry in entries
    )


def _load(path: Path) -> dict[str, Any] | None:
    if not path.exists():
        return {"version": 1, "hooks": {}}
    try:
        data = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as exc:
        lazy_header(ISSUE_ID)()
        LOGGER.warning(f"UNREADABLE: {path}: {exc} (--ignore {ISSUE_ID})")
        return None
    if not isinstance(data, dict):
        lazy_header(ISSUE_ID)()
        LOGGER.warning(f"INVALID: {path} is not a JSON object (--ignore {ISSUE_ID})")
        return None
    hooks = data.get("hooks")
    if hooks is not None and not isinstance(hooks, dict):
        lazy_header(ISSUE_ID)()
        LOGGER.warning(
            f"INVALID: {path} 'hooks' is not a JSON object (--ignore {ISSUE_ID})"
        )
        return None
    return data


def _missing_events(data: dict[str, Any]) -> list[str]:
    hooks = data.get("hooks")
    if not isinstance(hooks, dict):
        return list(CURSOR_EVENTS)
    missing: list[str] = []
    for event in CURSOR_EVENTS:
        entries = hooks.get(event)
        if not isinstance(entries, list) or not _has_command(entries, CURSOR_COMMAND):
            missing.append(event)
    return missing


def check_osc7501_hooks(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    if ISSUE_ID in ignore:
        return False
    path = target / CURSOR_HOOKS_PATH
    data = _load(path)
    if data is None:
        return True
    missing = _missing_events(data)
    if missing:
        lazy_header(ISSUE_ID)()
        LOGGER.warning(
            f"DRIFT: {path} missing {CURSOR_COMMAND!r} in "
            f"{', '.join(missing)} (--ignore {ISSUE_ID})"
        )
        return True
    if verbose:
        LOGGER.debug(f"OK: {path} has {CURSOR_COMMAND!r}")
    return False


def apply_osc7501_hooks(target: Path, *, verbose: bool) -> None:
    path = target / CURSOR_HOOKS_PATH
    data = _load(path)
    if data is None:
        return
    missing = _missing_events(data)
    if not missing:
        if verbose:
            LOGGER.debug(f"OK: {path} already has {CURSOR_COMMAND!r}")
        return
    hooks = data.setdefault("hooks", {})
    if not isinstance(hooks, dict):
        return
    data.setdefault("version", 1)
    for event in missing:
        entries = hooks.get(event)
        if not isinstance(entries, list):
            entries = []
            hooks[event] = entries
        entries.append({"command": CURSOR_COMMAND})
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(data, indent=2) + "\n")
    LOGGER.info(f"Updated {path}: added {CURSOR_COMMAND!r} to {', '.join(missing)}")
