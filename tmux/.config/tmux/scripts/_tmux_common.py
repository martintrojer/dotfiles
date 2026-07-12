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

import enum
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


# ---------- agent state enum ----------


class AgentState(enum.IntEnum):
    """Agent states ordered by urgency (higher = more urgent).

    The int value doubles as sort key for worst-state rollup.
    ``cleared`` is a transient signal, not a displayable state.
    """

    cleared = 0
    idle = 1
    working = 2
    blocked = 3
    crashed = 4

    @classmethod
    def parse(cls, raw: object, default: AgentState | None = None) -> AgentState | None:
        """Coerce a string/value to AgentState, returning *default* on miss."""
        if isinstance(raw, cls):
            return raw
        try:
            return cls[str(raw)]
        except (KeyError, ValueError):
            return default

    @property
    def visible(self) -> bool:
        """True for states that produce a badge / status-bar glyph."""
        return self in _VISIBLE_STATES


_VISIBLE_STATES = frozenset(
    {AgentState.working, AgentState.blocked, AgentState.crashed}
)

# Glyphs per state — single source for all display surfaces.
STATE_GLYPH: dict[AgentState, str] = {
    AgentState.crashed: "\u2717",  # ✗
    AgentState.blocked: "!",
    AgentState.working: "\u25b6",  # ▶
    AgentState.idle: "\u00b7",  # ·
}


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


# ---------- process tree agent detection ----------


class ProcessSnapshot:
    """One-shot ``ps`` snapshot for descendant-tree walks.

    Captures the full process table once; callers can then do multiple
    ``detect_agent`` lookups against different root pids without
    re-forking.  Used by ``status-window-label`` (per-window) and
    ``agent-attention`` (picker idle scan).
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


# ---------- agent state summary ----------


class AgentStats:
    """Counts of agent windows by state across all tmux sessions.

    Single source of truth for the status pill, tms picker, and any
    other consumer that needs to know how many agents are in each state.
    """

    __slots__ = ("_by_session", "blocked", "crashed", "idle", "total", "working")

    def __init__(
        self,
        *,
        crashed: int = 0,
        blocked: int = 0,
        working: int = 0,
        idle: int = 0,
    ) -> None:
        self.crashed = crashed
        self.blocked = blocked
        self.working = working
        self.idle = idle
        self.total = crashed + blocked + working + idle
        self._by_session: dict[str, AgentStats] = {}

    @property
    def attention_count(self) -> int:
        """Windows needing user action (blocked + crashed)."""
        return self.blocked + self.crashed

    @property
    def worst_state(self) -> AgentState | None:
        """Return the most urgent state, or None."""
        if self.crashed:
            return AgentState.crashed
        if self.blocked:
            return AgentState.blocked
        if self.working:
            return AgentState.working
        if self.idle:
            return AgentState.idle
        return None

    def per_session(self) -> dict[str, AgentStats]:
        """Return per-session breakdowns.  Requires a fresh ``scan()``."""
        return dict(self._by_session)


def scan_agent_states() -> AgentStats:
    """Scan all tmux windows for agent state.

    Reads ``@agent_state`` and ``@pane_agent`` in one ``list-windows``
    call.  Returns global counts and caches per-session breakdowns.
    """
    result = tmux_cmd(
        "list-windows",
        "-a",
        "-F",
        "#{session_name}\t#{@agent_state}\t#{@pane_agent}",
        check=False,
    )
    if result.returncode != 0:
        return AgentStats()

    global_counts: dict[AgentState, int] = {}
    by_session: dict[str, dict[AgentState, int]] = {}

    for line in result.stdout.splitlines():
        parts = line.split("\t")
        if len(parts) != 3:
            continue
        session_name, state_str, pane_agent = parts
        state = AgentState.parse(state_str)
        if state is None and pane_agent == "1":
            state = AgentState.idle
        if state is None:
            continue
        global_counts[state] = global_counts.get(state, 0) + 1
        sc = by_session.setdefault(session_name, {})
        sc[state] = sc.get(state, 0) + 1

    def _from_counts(counts: dict[AgentState, int]) -> AgentStats:
        return AgentStats(
            crashed=counts.get(AgentState.crashed, 0),
            blocked=counts.get(AgentState.blocked, 0),
            working=counts.get(AgentState.working, 0),
            idle=counts.get(AgentState.idle, 0),
        )

    stats = _from_counts(global_counts)
    stats._by_session = {
        name: _from_counts(counts) for name, counts in by_session.items()
    }
    return stats
