"""Shared Waybar script state."""

from __future__ import annotations

import os
from pathlib import Path

STATE_DIR = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local/state")) / "waybar"
DEMO_FILE = STATE_DIR / "demo"


def demo_enabled() -> bool:
    return DEMO_FILE.exists()


def set_demo(enabled: bool) -> None:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    if enabled:
        DEMO_FILE.write_text("1\n", encoding="utf-8")
    else:
        DEMO_FILE.unlink(missing_ok=True)
