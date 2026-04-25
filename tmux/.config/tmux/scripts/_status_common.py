"""Shared helpers for tmux status-bar render scripts.

Each status segment renderer (status-ai, status-ram, status-uptime, ...)
needs the same things:

  * A throttled error log so silent failures stay debuggable without
    spamming the disk on every tmux refresh.
  * A common idea of which exceptions are *expected* during rendering
    (OS / parsing / arithmetic) vs. programmer errors that should crash
    so the bug surfaces in development instead of silently turning into
    a `?` glyph in the status bar.

Centralising both means each renderer's ``main()`` collapses to a couple
of lines and the throttling policy stays consistent across segments.
"""

from __future__ import annotations

import os
import subprocess
import time
from pathlib import Path

# All status-bar logs land under one dir for easy discovery.
STATE_DIR = (
    Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local" / "state")
    / "tmux-status"
)

# Exceptions a status renderer can plausibly raise from OS / parsing /
# arithmetic. Anything outside this set is a programmer error and should
# crash loudly so the bug surfaces in normal development instead of
# silently turning into a ``?`` glyph in the tmux status bar.
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
    Writes one line per ``throttle_seconds`` window; further calls within
    the window are no-ops. All filesystem errors are swallowed — the
    logger must never itself break the status bar.
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
