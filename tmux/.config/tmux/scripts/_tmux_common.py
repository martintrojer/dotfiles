"""Shared helpers for tmux IPC across `tmux/.config/tmux/scripts/`.

Any script that shells out to the ``tmux`` CLI goes through this module
so the ``TMUX_SOCKET_NAME`` / ``TMUX_SOCKET_PATH`` convention (used by
the smoke-test harness in ``test-status-tools`` to pin the script under
test to an isolated tmux server) is honored consistently.

Sibling to ``_status_common.py``: that one owns the silent-renderer
error policy + throttled log used by ``status-*`` scripts; this one
owns the IPC plumbing used by status renderers *and* interactive
scripts (``tms``, ``cheatsheet``, ``agent-attention``).

The leading underscore matches the rest of the repo's convention for
library files colocated with executables (don't ``exec _tmux_common``).
"""

from __future__ import annotations

import os
import subprocess


def tmux_socket_args() -> tuple[str, ...]:
    """Return the ``-S <path>`` or ``-L <name>`` args appropriate for
    the current environment, or an empty tuple for the default socket.

    ``TMUX_SOCKET_PATH`` (preferred) and ``TMUX_SOCKET_NAME`` let test
    harnesses point a script at an isolated tmux server. Real
    interactive use leaves both unset.
    """
    socket_path = os.environ.get("TMUX_SOCKET_PATH")
    if socket_path:
        return ("-S", socket_path)
    socket_name = os.environ.get("TMUX_SOCKET_NAME")
    if socket_name:
        return ("-L", socket_name)
    return ()


def tmux_cmd(
    *args: str,
    check: bool = True,
    capture: bool = True,
) -> subprocess.CompletedProcess[str]:
    """Run a ``tmux`` command with socket-env honoring.

    Defaults match the typical script idiom: text mode, capture
    stdout/stderr, raise on non-zero exit. Pass ``check=False`` for
    callers that want to inspect ``returncode`` themselves (e.g. the
    ``tmux display-message`` lookups in ``agent-attention``, which
    tolerate "no such target" without raising).
    """
    return subprocess.run(
        ["tmux", *tmux_socket_args(), *args],
        check=check,
        text=True,
        capture_output=capture,
    )
