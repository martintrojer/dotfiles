"""Shared Waybar script state, subprocess helper, and failure policy.

Colocated with the executables: waybar invokes them directly, so Python's
``sys.path[0]`` makes ``from _state import ...`` resolve without PYTHONPATH.

The failure policy below is deliberately the same one
``tmux/.config/tmux/scripts/_status_common.py`` encodes, because both packages
have the identical constraint: a headless renderer re-runs every few seconds
with no terminal, so a traceback is invisible and losing stdout blanks the bar.
It is copied rather than imported -- the two live in different stow packages
and rely on ``sys.path[0]`` colocation (see docs/DECISIONS.md, "A shared
pylib/ helper module"). Keep the two comparable when either changes.
"""

from __future__ import annotations

import json
import os
import subprocess
import time
from collections.abc import Callable
from pathlib import Path

STATE_DIR = (
    Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local/state") / "waybar"
)
DEMO_FILE = STATE_DIR / "demo"

# Exceptions a Waybar renderer can plausibly raise from OS / subprocess /
# parsing / arithmetic. Anything outside this set is a programmer error and
# should crash loudly so the bug surfaces in normal development instead of
# silently turning into a blank module.
EXPECTED_ERRORS = (
    OSError,
    subprocess.SubprocessError,
    RuntimeError,
    ValueError,
    KeyError,
    ZeroDivisionError,
)

DEFAULT_THROTTLE_SECONDS = 60 * 60  # one breadcrumb per hour


def log_error(
    name: str,
    message: str,
    *,
    throttle_seconds: int = DEFAULT_THROTTLE_SECONDS,
) -> None:
    """Append a throttled error breadcrumb so silent failures stay debuggable.

    ``name`` becomes the log filename (``<name>.log``) under ``STATE_DIR``.
    Writes one line per ``throttle_seconds`` window; further calls within the
    window are no-ops. All filesystem errors are swallowed -- the logger must
    never itself break the bar.
    """
    log = STATE_DIR / f"{name}.log"
    try:
        STATE_DIR.mkdir(parents=True, exist_ok=True)
        if log.is_file() and time.time() - log.stat().st_mtime < throttle_seconds:
            return
        with log.open("a", encoding="utf-8") as handle:
            handle.write(f"{int(time.time())}\t{message}\n")
    except OSError:
        return


def guarded_render(
    name: str, render: Callable[[], int], fallback: dict[str, object]
) -> int:
    """Run a ``"return-type": "json"`` renderer, degrading to ``fallback``.

    Every module's JSON path goes through here so a failure prints a payload
    Waybar can parse (normally the module's own empty/collapsed state) instead
    of a traceback or nothing at all. Callers must print exactly once, at the
    end of ``render``, so a fallback can never append to partial output.
    """
    try:
        return render()
    except EXPECTED_ERRORS as exc:
        log_error(name, f"{name} render failed: {exc!r}")
        print(json.dumps(fallback, ensure_ascii=False))
        return 0


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
