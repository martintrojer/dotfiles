"""Keep the Cursor Agent settings this repo cares about in ~/.cursor/cli-config.json.

The file is not linked: the CLI writes auth, model choice, and caches into it,
so a tracked copy would churn and would commit account data. This repo owns
only the keys in CURSOR_SETTINGS. --check reports drift, --apply merges them
in, and every other key stays the CLI's.
"""

from __future__ import annotations

from pathlib import Path
from typing import Final

from .json_keys import JsonKeys

ISSUE_ID: Final[str] = "cursor-settings"
SETTINGS_PATH: Final[Path] = Path(".cursor") / "cli-config.json"

# "unrestricted" is Run Everything (--force / --yolo): auto-approve tool calls.
CURSOR_SETTINGS: Final[dict[str, object]] = {
    "approvalMode": "unrestricted",
}

_FILE: Final[JsonKeys] = JsonKeys(
    issue_id=ISSUE_ID,
    path=SETTINGS_PATH,
    owned=CURSOR_SETTINGS,
    trailing_newline=True,
)

check_cursor_settings = _FILE.check
apply_cursor_settings = _FILE.apply
