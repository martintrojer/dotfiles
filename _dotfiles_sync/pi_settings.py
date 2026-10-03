"""Keep the pi settings this repo cares about in ~/.pi/agent/settings.json.

The file is not linked: pi writes to it on every /model, /settings, and
version bump, so a tracked copy would churn on every pi update. This repo
owns only the keys in PI_SETTINGS. --check reports drift, --apply merges
them in, and every other key stays pi's.
"""

from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Final

from .config import lazy_header

LOGGER: Final[logging.Logger] = logging.getLogger("dotfiles-sync")

ISSUE_ID: Final[str] = "pi-settings"
SETTINGS_PATH: Final[Path] = Path(".pi") / "agent" / "settings.json"

PI_SETTINGS: Final[dict[str, object]] = {
    # On top of pi's default tools (read, bash, edit, write).
    "defaultTools": ["+codemode", "+tool_search"],
    # Not "regular": there, any tmux pane resize clears scrollback and
    # replays the whole transcript into the pane. Fullscreen redraws only
    # the visible rows, at the cost of tmux copy-mode not seeing pi.
    "tuiMode": "fullscreen",
    # Object values merge one level deep; other terminal.* keys stay pi's.
    "terminal": {"showTerminalProgress": True},
}


def _load(path: Path) -> dict[str, object] | None:
    """The current settings: {} when missing, None when unusable (already reported)."""
    if not path.exists():
        return {}
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
    return data


def _merged(current: object, want: object) -> object:
    """want laid over current; dicts merge one level, anything else replaces."""
    if isinstance(want, dict) and isinstance(current, dict):
        return {**current, **want}
    return want


def _drift(current: dict[str, object]) -> list[str]:
    return [
        key
        for key, want in PI_SETTINGS.items()
        if current.get(key) != _merged(current.get(key), want)
    ]


def check_pi_settings(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    if ISSUE_ID in ignore:
        return False
    path = target / SETTINGS_PATH
    current = _load(path)
    if current is None:
        return True
    drift = _drift(current)
    if drift:
        header = lazy_header(ISSUE_ID)
        for key in drift:
            header()
            LOGGER.warning(
                f"DRIFT: {path} {key}={json.dumps(current.get(key))} "
                f"want={json.dumps(PI_SETTINGS[key])} (--ignore {ISSUE_ID})"
            )
        return True
    if verbose:
        LOGGER.debug(f"OK: {path} has {', '.join(PI_SETTINGS)}")
    return False


def apply_pi_settings(target: Path, *, verbose: bool) -> None:
    path = target / SETTINGS_PATH
    current = _load(path)
    if current is None:
        return
    drift = _drift(current)
    if not drift:
        if verbose:
            LOGGER.debug(f"OK: {path} already has {', '.join(PI_SETTINGS)}")
        return
    current.update({key: _merged(current.get(key), PI_SETTINGS[key]) for key in drift})
    path.parent.mkdir(parents=True, exist_ok=True)
    # Same shape pi writes, so its next save is not a reformat.
    path.write_text(json.dumps(current, indent=2))
    LOGGER.info(f"Updated {path}: {', '.join(drift)}")
