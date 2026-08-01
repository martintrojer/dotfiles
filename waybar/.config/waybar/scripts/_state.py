"""Shared Waybar script state and subprocess helper.

Colocated with the executables: waybar invokes them directly, so Python's
``sys.path[0]`` makes ``from _state import ...`` resolve without PYTHONPATH.
"""

from __future__ import annotations

import os
import subprocess
from pathlib import Path

STATE_DIR = (
    Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local/state") / "waybar"
)
DEMO_FILE = STATE_DIR / "demo"


def run(
    cmd: list[str], *, input_text: str | None = None
) -> subprocess.CompletedProcess[str]:
    """Run ``cmd``; a missing binary yields rc 127 rather than a traceback.

    Waybar modules have no terminal, and several are ``"return-type": "json"``
    where a traceback on stderr plus no stdout just blanks the module. Report a
    missing binary the way a shell does and let the caller decide.
    """
    try:
        return subprocess.run(
            cmd, input=input_text, capture_output=True, text=True, check=False
        )
    except FileNotFoundError:
        return subprocess.CompletedProcess(cmd, 127, "", f"command not found: {cmd[0]}")


def demo_enabled() -> bool:
    return DEMO_FILE.exists()


def set_demo(enabled: bool) -> None:
    STATE_DIR.mkdir(parents=True, exist_ok=True)
    if enabled:
        DEMO_FILE.write_text("1\n", encoding="utf-8")
    else:
        DEMO_FILE.unlink(missing_ok=True)
