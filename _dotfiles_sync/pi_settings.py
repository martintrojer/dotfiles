"""Keep the pi settings this repo cares about in ~/.pi/agent/settings.json.

The file is not linked: pi writes to it on every /model, /settings, and
version bump, so a tracked copy would churn on every pi update. This repo
owns only the keys in PI_SETTINGS. --check reports drift, --apply merges
them in, and every other key stays pi's.
"""

from __future__ import annotations

from pathlib import Path
from typing import Final

from .json_keys import JsonKeys

ISSUE_ID: Final[str] = "pi-settings"
SETTINGS_PATH: Final[Path] = Path(".pi") / "agent" / "settings.json"

PI_SETTINGS: Final[dict[str, object]] = {
    # On top of pi's default tools (read, bash, edit, write).
    "defaultTools": ["+codemode"],
    # Not "regular": there, any tmux pane resize clears scrollback and
    # replays the whole transcript into the pane. Fullscreen redraws only
    # the visible rows, at the cost of tmux copy-mode not seeing pi.
    "tuiMode": "fullscreen",
    # Object values merge one level deep; other terminal.* keys stay pi's.
    "terminal": {"showTerminalProgress": True},
}

_FILE: Final[JsonKeys] = JsonKeys(
    issue_id=ISSUE_ID,
    path=SETTINGS_PATH,
    owned=PI_SETTINGS,
)

check_pi_settings = _FILE.check
apply_pi_settings = _FILE.apply
