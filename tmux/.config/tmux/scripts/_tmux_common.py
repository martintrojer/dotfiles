"""Shared helpers for tmux IPC across `tmux/.config/tmux/scripts/`.

Any script that shells out to the ``tmux`` CLI goes through this module
so the ``TMUX_SOCKET_NAME`` / ``TMUX_SOCKET_PATH`` convention (used by
the smoke-test harness in ``test-status-tools`` to pin the script under
test to an isolated tmux server) is honored consistently.

Sibling to ``_status_common.py``: that one owns the silent-renderer
error policy + throttled log used by ``status-*`` scripts; this one
owns the IPC plumbing used by ``status-window-label`` and ``cheatsheet``.
Agent state is murmur's; mu-crew/dotfiles renders it.

The leading underscore matches the rest of the repo's convention for
library files colocated with executables (don't ``exec _tmux_common``).
"""

from __future__ import annotations

import os
import subprocess
from collections import deque

# Known agent command basenames.  Override via env var (space-separated).
AGENT_COMMANDS: tuple[str, ...] = tuple(
    os.environ.get("TMUX_LABEL_AGENTS", "codex pi opencode").split()
)
# Interpreter wrappers that may host an agent child process.
AGENT_WRAPPERS: tuple[str, ...] = tuple(
    os.environ.get(
        "TMUX_LABEL_AGENT_WRAPPERS",
        "Python python python3 node nodejs bun deno",
    ).split()
)


# ---------- tmux IPC ----------


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
    callers that want to inspect ``returncode`` themselves (target
    lookups that tolerate "no such target" without raising).
    """
    return subprocess.run(
        ["tmux", *tmux_socket_args(), *args],
        check=check,
        text=True,
        capture_output=capture,
    )


# ---------- process tree agent detection ----------


class ProcessSnapshot:
    """One-shot ``ps`` snapshot for descendant-tree walks.

    Captures the full process table once; callers can then do multiple
    ``detect_agent`` lookups against different root pids without
    re-forking.  Used by ``status-window-label`` (per-window).
    """

    def __init__(self) -> None:
        self.children: dict[str, list[str]] = {}
        self.comm: dict[str, str] = {}
        self._loaded = False

    def _load(self) -> None:
        if self._loaded:
            return
        self._loaded = True
        try:
            res = subprocess.run(
                ["ps", "-axo", "pid=,ppid=,comm="],
                capture_output=True,
                text=True,
                check=False,
                timeout=5,
            )
        except (OSError, subprocess.SubprocessError):
            return
        if res.returncode != 0:
            return
        for line in res.stdout.splitlines():
            parts = line.split(None, 2)
            if len(parts) < 3:
                continue
            pid, ppid, cmd = parts
            self.comm[pid] = cmd.rsplit("/", 1)[-1]
            self.children.setdefault(ppid, []).append(pid)

    def detect_agent(
        self,
        root_pid: str,
        agents: tuple[str, ...] = AGENT_COMMANDS,
    ) -> str | None:
        """Walk descendants of *root_pid*; return first matching agent basename."""
        if not root_pid:
            return None
        self._load()
        wanted = frozenset(agents)
        queue: deque[str] = deque([root_pid])
        seen: set[str] = set()
        while queue:
            cur = queue.popleft()
            if cur in seen:
                continue
            seen.add(cur)
            if self.comm.get(cur) in wanted:
                return self.comm[cur]
            queue.extend(self.children.get(cur, ()))
        return None
