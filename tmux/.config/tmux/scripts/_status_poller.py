"""Shared plumbing for tmux status-bar *poller* scripts.

The pattern: poll something slow in the background, publish a cached
snapshot, and let the status bar render it for free. The poller-side
boilerplate — singleton flock, atomic state write, rotating log,
tmux-server liveness gating, lazy respawn from the renderer, config
loading, signal handling — is generic across every instance of it, so
it lives here and each poller keeps only its source-specific bits.

Why it exists at all: ``status-right`` is re-rendered far more often
than ``status-interval`` suggests (tmux also redraws on pane and window
events — a 14-window session was measured running one segment 36 times
in 30s, against the 6 a 5s interval implies). Anything expensive on
that path is therefore multiplied by an event rate you do not control.
And the unit of cost is the *process*, not the milliseconds: an
interpreter start loads hundreds of shared libraries before running a
line of your code, and anything auditing process starts multiplies that
again. So "expensive" means forks, not runtime. Moving the work behind
a poller decouples the two rates; see ``docs/DECISIONS.md``.

Design rules followed here:

* **Free functions, paths threaded in.** No classes, no implicit
  state. Each caller passes its own ``state_dir`` / ``state_file`` /
  ``pid_file`` etc. This is the same pattern ``_status_common`` uses
  one rung down (throttled error log + EXPECTED_ERRORS tuple); we sit
  on top of that for the synchronous-render error path.
* **No source assumptions.** Nothing here knows what is being polled.
* **Test surface preserved through re-exports.** The
  ``test-status-tools`` suite pokes names like ``poller.tmux_status``
  directly, so callers may re-export them as thin wrappers.

Layout:

  1. Paths / time helpers
  2. Logging setup (rotating)
  3. Config loading for any frozen ``@dataclass``
  4. Singleton flock (``acquire_singleton``)
  5. State snapshot read/write
  6. Renderer-side lazy respawn (``LazySpawner``)
  7. tmux client awareness (``tmux_status``)
  8. pgrep-based stray-poller management
  9. Signal handlers
 10. Small style helpers (``style_block``, ``as_int``) used by renderers
 11. SQLite helpers for pollers that cache into a local DB
"""

from __future__ import annotations

import contextlib
import dataclasses
import errno
import fcntl
import json
import logging
import logging.handlers
import os
import shutil
import signal
import sqlite3
import subprocess
import sys
import time
from collections.abc import Callable
from pathlib import Path
from typing import Any

# ``__all__`` is grouped by function area below — paths, logging,
# config, etc. — not alphabetically. Reading-order legibility beats
# RUF022's machine sort for a module this size; the noqa is
# intentional and applies to the *order*, not the contents.
__all__ = [  # noqa: RUF022 (intentionally grouped, not sorted)
    # paths
    "xdg_state_dir",
    "xdg_config_dir",
    # logging
    "setup_rotating_logger",
    # config
    "load_dataclass_config",
    # singleton
    "acquire_singleton",
    # state
    "write_state_atomic",
    "write_render_atomic",
    "read_state_snapshot",
    # sqlite
    "sqlite_connect",
    "sqlite_meta_get",
    "sqlite_meta_set",
    # respawn
    "LazySpawner",
    "MIN_SPAWN_INTERVAL_SECONDS",
    # tmux awareness
    "TMUX_ATTACHED",
    "TMUX_DETACHED",
    "TMUX_GONE",
    "tmux_status",
    "tmux_server_fingerprint",
    "tmux_cmd",
    # pgrep / kill
    "find_pollers_by_argv",
    "kill_pid",
    # signal
    "install_signal_handlers",
    # render helpers
    "style_block",
    "as_int",
]


# Default crash-loop respawn rate limit. Tuned shorter than the
# "stale" threshold a renderer uses (typically ~5 min) so a flaky
# poller still self-heals visibly to the user — badge goes "?" for at
# most one cycle — but long enough that a hard-crashing script does
# not burn CPU on respawns at the tmux status-interval cadence (5s by
# default). Callers can override via ``LazySpawner(...,
# min_spawn_interval_seconds=...)`` if their poller has a faster /
# slower failure characteristic.
MIN_SPAWN_INTERVAL_SECONDS = 30


# ---------------------------------------------------------------------------
# 1. Paths
# ---------------------------------------------------------------------------


def xdg_state_dir(name: str) -> Path:
    """Return ``$XDG_STATE_HOME/<name>`` with the standard fallback.

    Centralised so every poller derives its state dir identically; a
    typo in one of N pollers would otherwise hide files in an
    unexpected place. ``name`` is the per-poller subdir, e.g.
    ``tmux-gchat`` or ``tmux-meta``.
    """
    base = Path(os.environ.get("XDG_STATE_HOME") or Path.home() / ".local" / "state")
    return base / name


def xdg_config_dir(name: str) -> Path:
    """Return ``$XDG_CONFIG_HOME/<name>`` with the standard fallback."""
    base = Path(os.environ.get("XDG_CONFIG_HOME") or Path.home() / ".config")
    return base / name


# ---------------------------------------------------------------------------
# 2. Logging
# ---------------------------------------------------------------------------


def setup_rotating_logger(
    name: str,
    log_file: Path,
    *,
    max_bytes: int = 256 * 1024,
    backup_count: int = 2,
) -> logging.Logger:
    """Build a rotating file logger so the loop is debuggable but never floods disk.

    The logger is *idempotent*: repeated calls with the same ``name``
    do not stack handlers, which matters because a developer iterating
    on the poller may import it from multiple test contexts. Bounded
    at ~256 KB across three rotated files by default.

    Propagation is disabled so the sync renderer (which has its own
    throttled error log via ``_status_common``) does not inherit our
    handler if it ever imports the poller module.
    """
    log_file.parent.mkdir(parents=True, exist_ok=True)
    logger = logging.getLogger(name)
    logger.setLevel(logging.INFO)
    if not logger.handlers:
        handler = logging.handlers.RotatingFileHandler(
            log_file,
            maxBytes=max_bytes,
            backupCount=backup_count,
            encoding="utf-8",
        )
        handler.setFormatter(logging.Formatter("%(asctime)s %(levelname)s %(message)s"))
        logger.addHandler(handler)
        logger.propagate = False
    return logger


# ---------------------------------------------------------------------------
# 3. Config loading (generic over any frozen @dataclass)
# ---------------------------------------------------------------------------


def load_dataclass_config(
    # type[Any], not type: callers pass a concrete frozen dataclass, and
    # dataclasses.fields() only accepts DataclassInstance. A bare `type`
    # cannot satisfy that protocol, so ty rejects the call.
    cls: type[Any],
    config_file: Path,
    *,
    logger: logging.Logger | None = None,
) -> object:
    """Load a JSON config file and shallow-merge over ``cls()`` defaults.

    Generic over any frozen ``@dataclass``: declared fields drive type
    coercion (``str`` / ``bool`` / ``int`` / ``float`` /
    ``tuple[str,...]``), anything else flows through unchanged. Bad
    config (parse error, wrong type, unknown key) degrades to defaults
    with a logged warning rather than crashing the loop — a poller
    running with "almost right" filters is far more useful than no
    poller at all.

    The shallow-merge contract matches what every gchat-style ``Config``
    needs: scalar fields and flat tuples of strings. Nothing nested,
    so no deep-merge logic is needed. If a future caller wants nested
    config, they should layer their own loader on top of this one
    rather than complicate the shared path.
    """
    log = logger or logging.getLogger("status-poller")

    field_types = {f.name: f.type for f in dataclasses.fields(cls)}

    if not config_file.exists():
        return cls()
    try:
        with config_file.open("r", encoding="utf-8") as f:
            user = json.load(f)
    except (OSError, json.JSONDecodeError) as exc:
        log.warning("ignoring %s: %s", config_file, exc)
        return cls()
    if not isinstance(user, dict):
        log.warning("ignoring %s: top level is not an object", config_file)
        return cls()

    overrides: dict[str, object] = {}
    for key, value in user.items():
        if key.startswith("_"):
            # Comment-style keys ("_comment": "...") are allowed and ignored.
            continue
        if key not in field_types:
            log.warning("ignoring unknown config key %r", key)
            continue
        try:
            overrides[key] = _coerce_field(key, value, field_types[key])
        except (TypeError, ValueError) as exc:
            log.warning("ignoring bad value for %r: %s", key, exc)
    try:
        return dataclasses.replace(cls(), **overrides)
    except TypeError as exc:
        log.warning("ignoring config overrides: %s", exc)
        return cls()


def _coerce_field(name: str, value: object, declared: object) -> object:
    """Coerce a JSON-loaded value to a dataclass field's declared type.

    Only the shapes that actually appear in poller ``Config`` classes
    are handled (str, bool, int, float, tuple[str, ...]). Anything
    else falls through and will surface from the eventual
    ``replace(...)`` call. Decl is stringified because Python preserves
    annotations as strings under ``from __future__ import annotations``,
    which all the poller modules use.
    """
    decl = str(declared)
    if decl.startswith("tuple["):
        if not isinstance(value, (list, tuple)):
            raise TypeError(f"{name} must be a list")
        return tuple(str(v) for v in value)
    if decl == "str":
        return str(value)
    if decl == "bool":
        return bool(value)
    if decl == "int":
        if not isinstance(value, (int, float, str)):
            raise TypeError(f"expected number/str, got {type(value).__name__}")
        return int(value)
    if decl == "float":
        if not isinstance(value, (int, float, str)):
            raise TypeError(f"expected number/str, got {type(value).__name__}")
        return float(value)
    return value


# ---------------------------------------------------------------------------
# 4. Singleton flock
# ---------------------------------------------------------------------------


def acquire_singleton(pid_file: Path) -> int | None:
    """Take an exclusive flock on ``pid_file`` and write our pid into it.

    Returns the open fd on success (caller must hold it for the
    lifetime of the process — the lock is released by the kernel when
    the fd is closed) or ``None`` if another poller already holds the
    lock. Intentionally does *not* unlink the pid file on exit: the
    next invocation overwrites it, and a stale file with a dead pid is
    fine because the renderer's lazy-spawn path checks pid liveness
    before deciding to spawn.
    """
    pid_file.parent.mkdir(parents=True, exist_ok=True)
    fd = os.open(pid_file, os.O_RDWR | os.O_CREAT, 0o600)
    try:
        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError as exc:
        os.close(fd)
        if exc.errno in (errno.EAGAIN, errno.EWOULDBLOCK):
            return None
        raise
    # Holding the lock; (over)write the pid. truncate-then-write so a
    # stale longer pid does not leave digits behind.
    os.ftruncate(fd, 0)
    os.lseek(fd, 0, os.SEEK_SET)
    os.write(fd, f"{os.getpid()}\n".encode("ascii"))
    return fd


# ---------------------------------------------------------------------------
# 5. State snapshot
# ---------------------------------------------------------------------------


def write_state_atomic(state_file: Path, payload: dict[str, object]) -> None:
    """Atomic snapshot write so the renderer never sees a half-written file.

    ``tmp + os.replace`` is the standard recipe; on POSIX the rename
    is atomic, on Windows ``os.replace`` is the documented atomic
    primitive. Forces UTF-8 because every renderer reads with
    ``encoding="utf-8"``.
    """
    state_file.parent.mkdir(parents=True, exist_ok=True)
    tmp = state_file.with_suffix(state_file.suffix + ".tmp")
    tmp.write_text(json.dumps(payload, ensure_ascii=False), encoding="utf-8")
    os.replace(tmp, state_file)


def write_render_atomic(render_file: Path, text: str) -> None:
    """Atomically publish a pre-rendered status fragment for the sh renderer.

    The poller computes the full tmux ``status-right`` fragment (styled
    ``#[...]`` blocks and all) and drops it here next to ``state.json``.
    The per-tick shell renderer then only has to ``cat`` this file --
    no interpreter cold-start, no shared-library storm, no JSON parse. ``tmp + os.replace`` keeps the reader from ever seeing
    a half-written fragment, identical to ``write_state_atomic``.

    A trailing newline is intentionally *not* added: tmux renders the
    file's exact bytes, and a stray newline would shift the bar.
    """
    render_file.parent.mkdir(parents=True, exist_ok=True)
    tmp = render_file.with_suffix(render_file.suffix + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    os.replace(tmp, render_file)


def read_state_snapshot(state_file: Path) -> dict[str, object] | None:
    """Read ``state_file`` as a JSON object, returning ``None`` on any failure.

    Both "no snapshot yet" and "snapshot unreadable" collapse to
    ``None`` so the renderer treats them identically: render nothing
    rather than a misleading partial badge. The caller is expected to
    log via its own throttled breadcrumb if it cares about
    distinguishing the two cases.
    """
    try:
        with state_file.open("r", encoding="utf-8") as f:
            data = json.load(f)
    except FileNotFoundError:
        return None
    except (OSError, json.JSONDecodeError):
        return None
    return data if isinstance(data, dict) else None


# ---------------------------------------------------------------------------
# 6. Renderer-side lazy respawn
# ---------------------------------------------------------------------------


class LazySpawner:
    """Lazy respawn helper used by sync status renderers.

    Bundles the four pieces of state a renderer needs in order to
    self-heal the background poller without an external supervisor:

    * ``pid_file`` — written by the poller's flock holder; the
      renderer reads it to check ``pid_alive``.
    * ``stamp_file`` — empty marker whose mtime is the wall-clock of
      the last spawn attempt; used to rate-limit crash-loop respawns.
    * ``poller_path`` — the sibling executable to ``Popen``.
    * ``min_interval`` — refuse to respawn more often than this.

    Three guards stack inside ``ensure()``:

    1. ``pid_alive(read_pid())`` — cheap, lock-free, skips the spawn
       while a previous poller is healthy.
    2. ``last_spawn_age() < min_interval`` — file-based rate limit
       protecting against hard-crashing scripts getting respawned
       every 5s by the tmux status loop.
    3. ``fcntl.flock`` inside the poller itself — authoritative
       singleton, so even two near-simultaneous spawns (one wins the
       rate-limit check) still produce only one running loop.

    The spawn uses ``start_new_session=True`` + ``close_fds`` so the
    child is reparented to launchd (macOS) / init-like reaper (Linux);
    that means zombies are impossible because the host reaper handles
    reparented orphans.
    """

    def __init__(
        self,
        *,
        pid_file: Path,
        stamp_file: Path,
        poller_path: Path,
        min_interval_seconds: int = MIN_SPAWN_INTERVAL_SECONDS,
        log_callback: Callable[[str], object] | None = None,
    ) -> None:
        self.pid_file = pid_file
        self.stamp_file = stamp_file
        self.poller_path = poller_path
        self.min_interval_seconds = min_interval_seconds
        # Optional callback(str) for spawn-failure breadcrumbs. The
        # gchat renderer already wires this to ``_status_common.log_error``
        # so missing CLIs surface in the throttled error log instead
        # of vanishing into stderr.
        self._log_callback = log_callback

    # ---- pid file ----

    @staticmethod
    def pid_alive(pid: int) -> bool:
        """Return True when a process with ``pid`` is alive and reachable.

        ``os.kill(pid, 0)`` is the standard liveness probe: it never
        sends a real signal but does the permission/existence check.
        Both "no such process" (ESRCH) and "not permitted" (EPERM) are
        treated as "not the process we spawned", since a recycled pid
        owned by some other user would never be writing our state file
        anyway.
        """
        if pid <= 0:
            return False
        try:
            os.kill(pid, 0)
        except ProcessLookupError:
            return False
        except PermissionError:
            return False
        return True

    def read_pid(self) -> int:
        try:
            raw = self.pid_file.read_text(encoding="utf-8").strip()
            return int(raw) if raw else 0
        except (OSError, ValueError):
            return 0

    # ---- spawn rate limit ----

    def last_spawn_age(self) -> float:
        """Seconds since the last spawn attempt, or ``inf`` if never spawned.

        File-based so the rate limit survives across the
        (intentionally short-lived) renderer invocations: every
        status-bar tick is a fresh python process, so we cannot rely
        on in-memory state.
        """
        try:
            return time.time() - self.stamp_file.stat().st_mtime
        except FileNotFoundError:
            return float("inf")
        except OSError:
            return float("inf")

    def mark_spawn_attempt(self) -> None:
        """Touch the stamp file so the next status tick sees a fresh mtime."""
        try:
            self.stamp_file.parent.mkdir(parents=True, exist_ok=True)
            self.stamp_file.touch()
            # touch() is a no-op on existing files for ctime; force mtime.
            now = time.time()
            os.utime(self.stamp_file, (now, now))
        except OSError as exc:
            # Best-effort: if we can't write the stamp the rate limit
            # degrades to "spawn every tick" but spawn itself still
            # works.
            self._log(f"could not touch spawn stamp: {exc}")

    # ---- read state ----

    def read_state(self, state_file: Path) -> dict[str, object] | None:
        """Convenience wrapper for the renderer's "give me the snapshot" call.

        Routed through ``read_state_snapshot`` so future renderers get
        the same defensive read path for free.
        """
        return read_state_snapshot(state_file)

    # ---- spawn ----

    def ensure(self) -> None:
        """Spawn the poller as a detached process if not already running."""
        if self.pid_alive(self.read_pid()):
            return
        if not self.poller_path.exists():
            return
        if self.last_spawn_age() < self.min_interval_seconds:
            return
        self.mark_spawn_attempt()
        try:
            subprocess.Popen(
                [sys.executable, str(self.poller_path)],
                stdin=subprocess.DEVNULL,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
                close_fds=True,
                start_new_session=True,
            )
        except (OSError, subprocess.SubprocessError) as exc:
            self._log(f"could not spawn poller: {exc}")

    def _log(self, message: str) -> None:
        if self._log_callback is not None:
            self._log_callback(message)


# ---------------------------------------------------------------------------
# 7. tmux client awareness
# ---------------------------------------------------------------------------


# Distinct tri-state for the loop: server up + at least one client
# ("attached"), server up but everyone detached ("detached"), or no
# server at all ("gone"). The loop uses these to decide between the
# normal interval, the idle interval, and exiting outright.
TMUX_ATTACHED = "attached"
TMUX_DETACHED = "detached"
TMUX_GONE = "gone"

# Strings tmux prints to stderr when the server socket is missing.
# Matched case-insensitively, substring-only, so a translated build or
# trivial wording change still classifies correctly.
_TMUX_GONE_MARKERS = ("no server running", "no such file", "error connecting")


def tmux_cmd(args: list[str]) -> list[str]:
    """Build a ``tmux ...`` argv that honours TMUX_SOCKET_NAME / _PATH.

    Real shell sessions never set the socket env vars, so the default
    tmux server is selected as usual; the test harness sets
    ``TMUX_SOCKET_NAME`` to point at its isolated ``-L`` socket so
    smoke tests never poke the user's live session.
    """
    cmd = ["tmux"]
    socket_path = os.environ.get("TMUX_SOCKET_PATH")
    socket_name = os.environ.get("TMUX_SOCKET_NAME")
    if socket_path:
        cmd += ["-S", socket_path]
    elif socket_name:
        cmd += ["-L", socket_name]
    return cmd + args


def tmux_status() -> str:
    """Classify the tmux server's state for loop-control purposes.

    Returns one of ``TMUX_ATTACHED`` / ``TMUX_DETACHED`` / ``TMUX_GONE``.
    Ambiguous failures (binary missing, command timeout, unknown
    stderr) are reported as ``TMUX_ATTACHED`` so a transient hiccup
    keeps the badge updating instead of silently killing the poller —
    false positives here cost nothing, false negatives cost the user a
    stale badge.
    """
    if shutil.which("tmux") is None:
        return TMUX_ATTACHED
    try:
        proc = subprocess.run(
            tmux_cmd(["list-clients", "-F", "#{client_name}"]),
            capture_output=True,
            text=True,
            timeout=2,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return TMUX_ATTACHED
    if proc.returncode != 0:
        stderr = (proc.stderr or "").lower()
        if any(marker in stderr for marker in _TMUX_GONE_MARKERS):
            return TMUX_GONE
        # Non-zero exit we do not recognise: be conservative and assume
        # the server is fine, the user will see no behaviour change.
        return TMUX_ATTACHED
    return TMUX_ATTACHED if proc.stdout.strip() else TMUX_DETACHED


def tmux_server_fingerprint() -> str | None:
    """Identify *this specific* tmux server instance, or ``None`` if gone.

    Returns ``"<pid>:<start_time>"`` for the server on the active
    socket. The pair is what makes a restart detectable: a server
    killed and re-created on the *same socket* (reboot, crash+respawn,
    ``kill-server`` then ``new-session``) gets a fresh pid and
    start_time, so a poller that captured the old fingerprint at
    startup sees the value change and can exit.

    ``tmux_status`` alone cannot catch this case -- ``list-clients``
    succeeds against the replacement server, so the loop would happily
    keep polling on behalf of a server it was never spawned for until
    the 24h lifetime cap. The fingerprint closes that gap.

    ``None`` is returned for both "no server" and any ambiguous
    failure (binary missing, timeout, unparseable output). Callers
    must treat ``None`` as "don't know" and *not* as a change signal,
    so a transient hiccup never kills a healthy poller -- same
    conservative bias as :func:`tmux_status`.
    """
    if shutil.which("tmux") is None:
        return None
    try:
        proc = subprocess.run(
            tmux_cmd(["display-message", "-p", "#{pid}:#{start_time}"]),
            capture_output=True,
            text=True,
            timeout=2,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return None
    if proc.returncode != 0:
        return None
    out = proc.stdout.strip()
    # Expect "<digits>:<digits>"; reject anything else as ambiguous.
    if not out or ":" not in out:
        return None
    pid_part, _, start_part = out.partition(":")
    if not (pid_part.isdigit() and start_part.isdigit()):
        return None
    return out


# ---------------------------------------------------------------------------
# 8. pgrep / kill helpers
# ---------------------------------------------------------------------------


# Characters that mean an argv token is a shell command body, not
# a bare file path. POSIX paths never contain whitespace, ``;``,
# ``|``, ``&``, ``<``, or ``>`` (technically they *can* if escaped,
# but ``/proc/<pid>/cmdline`` exposes them literally pre-shell-parse,
# so a real path token never has them either). This lets
# ``find_pollers_by_argv`` reject the multi-line ``-c`` bodies a
# wrapper shell exposes when its last word happens to end with the
# needle.
_PATH_INVALID_CHARS = frozenset(" \t\n\r;|&<>")


def _is_path_like(token: str) -> bool:
    """True when ``token`` could plausibly be a real file path.

    Used by ``find_pollers_by_argv`` to filter out shell ``-c``
    bodies whose last component happens to *end with* the needle.
    A shell command string almost always contains at least one of
    ``" ;|&<>"`` or a newline; a bare path almost never does.
    """
    if not token:
        return False
    return not any(c in _PATH_INVALID_CHARS for c in token)


# Pids that ``find_pollers_by_argv`` must NEVER return regardless of
# argv match: our own pid (obvious), every ancestor up to PID 1, and
# our process group. The intent of ``--restart`` is to kill
# background daemons, not the wrapper infrastructure that launched
# the command — so the exclusion set extends *all the way up* the
# ancestry. Walking the full chain (rather than just parent +
# grandparent) handles arbitrarily deep wrapper stacks: pi's
# coding-agent harness today is roughly
# ``solo -> pi -> /bin/zsh -c '... python3 .../status-gchat-poller'
# -> python3 .../status-gchat-poller``, but a future CI runner or
# IDE-launcher could add more layers. A fixed-depth walk would
# silently regress when that happens; walking to PID 1 is bounded
# (process trees rarely exceed ~20 layers), cheap, and correct
# regardless of wrapper depth.
def _self_protected_pids() -> set[int]:
    own_pid = os.getpid()
    protected = {own_pid}
    # Process group: a sibling that shares our pgid is part of the
    # same logical invocation as us (e.g. another stage of a
    # pipeline). Treat as protected.
    with contextlib.suppress(OSError):
        protected.add(os.getpgid(0))
    # Walk the full ancestor chain. Stop at PID 1 (init) or when
    # the parent file is unreadable (e.g. parent already reaped).
    # Hard cap on iterations as belt-and-braces against a /proc
    # whose PPid field somehow points in a cycle.
    pid = os.getppid()
    for _ in range(64):
        if pid <= 1:
            break
        protected.add(pid)
        try:
            with open(f"/proc/{pid}/status") as f:
                for line in f:
                    if line.startswith("PPid:"):
                        try:
                            pid = int(line.split(maxsplit=1)[1])
                        except (IndexError, ValueError):
                            pid = 0
                        break
                else:
                    # No PPid line at all — likely a non-Linux
                    # /proc layout; stop walking.
                    break
        except OSError:
            break
    return protected


def find_pollers_by_argv(
    needle: str,
    *,
    fallback_pid_file: Path | None = None,
) -> list[int]:
    """Return every running pid whose argv has a token basenamed ``needle``.

    Matches by *path basename of a path-like argv token*, so a
    process is a hit only if ``needle`` appears as the basename of
    one of its argv slots AND that slot looks like a bare file
    path (no shell metacharacters). That means ``python3
    /path/to/status-gchat-poller`` matches (argv[1] basename ==
    ``status-gchat-poller``) but a shell whose ``-c`` body is a
    multi-line command that happens to *end with* the needle does
    **not** match — critical, because ``--restart`` is allowed to
    SIGTERM whatever we return, and a too-loose match could take
    down the user's parent shell or the agent harness's wrapper
    process.

    The exclusion is two-layered: ``_is_path_like`` rejects shell
    bodies on the argv-content axis, and ``_self_protected_pids``
    rejects our own pid + parent + grandparent + process group on
    the ancestry axis. Both are necessary because the failure mode
    has been observed in two distinct shapes:

      1. A wrapper shell with the needle as a substring of its
         ``-c`` body (rejected by ``_is_path_like``).
      2. A wrapper *python* invocation — e.g. pi's coding-agent
         harness running ``python3 tmux/.config/tmux/scripts/
         status-gchat-poller --restart`` — where the script-path
         argv token *is* path-like and *does* match by basename,
         because it's literally one of our own ancestor processes'
         pre-exec argv slots (rejected by ``_self_protected_pids``).

    We deliberately do not use ``pgrep -f``: ``-f`` matches against
    the full argv as a single string, which has the same shell-body
    footgun as a naive ``os.path.basename`` match. Walking
    ``/proc/*/cmdline`` and splitting on NUL gives us per-token
    control. On macOS ``/proc`` does not exist; we fall back to
    ``ps -A -o pid=,args=`` and apply the same per-token,
    path-like-only check.

    If neither ``/proc`` nor ``ps`` is available the caller can pass
    a ``fallback_pid_file`` and we fall back to its contents (better
    than nothing on the rare systems with neither).
    """
    protected = _self_protected_pids()
    proc_root = Path("/proc")

    # --- Linux: scan /proc per-token. Robust, no shell-quoting issues,
    # no pgrep regex portability traps. ---
    if proc_root.is_dir():
        pids: list[int] = []
        for entry in proc_root.iterdir():
            if not entry.name.isdigit():
                continue
            pid = int(entry.name)
            if pid in protected:
                continue
            try:
                raw = (entry / "cmdline").read_bytes()
            except (OSError, PermissionError):
                continue
            if not raw:
                # Kernel threads have empty cmdline; skip.
                continue
            tokens = raw.split(b"\x00")
            for tok in tokens:
                if not tok:
                    continue
                try:
                    text = tok.decode("utf-8", "replace")
                except UnicodeDecodeError:
                    continue
                if _is_path_like(text) and os.path.basename(text) == needle:
                    pids.append(pid)
                    break
        return pids

    # --- macOS / no-/proc fallback: parse ``ps -A -o pid=,args=``
    # ourselves and apply the same per-token basename match as the
    # /proc path. ``ps`` on BSD-derived systems uses *whitespace*
    # between argv tokens; that loses tokens-with-spaces fidelity, but
    # our needle is a script basename (no spaces) so the lossiness is
    # harmless. Using ``ps`` instead of ``pgrep -f`` is what keeps the
    # match basename-anchored: a shell whose argv body *contains* the
    # needle as a substring never appears as a standalone token. ---
    ps_bin = shutil.which("ps")
    if ps_bin is None:
        if fallback_pid_file is None:
            return []
        try:
            raw_pid = fallback_pid_file.read_text(encoding="utf-8").strip()
            pid = int(raw_pid) if raw_pid else 0
        except (OSError, ValueError):
            return []
        return [pid] if pid > 0 and pid not in protected else []

    try:
        proc = subprocess.run(
            [ps_bin, "-A", "-o", "pid=,args="],
            capture_output=True,
            text=True,
            timeout=5,
            check=False,
        )
    except (OSError, subprocess.SubprocessError):
        return []

    pids = []
    for line in (proc.stdout or "").splitlines():
        line = line.strip()
        if not line:
            continue
        # First whitespace splits pid from the args column. Tokens
        # within args are whitespace-separated (BSD ps convention).
        head, _, rest = line.partition(" ")
        try:
            pid = int(head)
        except ValueError:
            continue
        if pid <= 0 or pid in protected:
            continue
        # ``ps -A -o args=`` whitespace-splits the argv slots, so a
        # token at this layer is already without spaces. The
        # path-like check still filters out shell-body fragments
        # that contain ``;|&<>`` even after splitting.
        tokens = rest.split()
        if any(
            _is_path_like(tok) and os.path.basename(tok) == needle for tok in tokens
        ):
            pids.append(pid)
    return pids


def kill_pid(pid: int, timeout_seconds: float = 2.0) -> bool:
    """SIGTERM a single pid; SIGKILL if it does not exit in time.

    Returns True if the process is gone after the call, False if we
    could not reach it at all (ESRCH / EPERM). Idempotent.
    """
    try:
        os.kill(pid, signal.SIGTERM)
    except ProcessLookupError:
        return True  # already gone
    except PermissionError:
        return False  # different user, leave alone
    deadline = time.monotonic() + timeout_seconds
    while time.monotonic() < deadline:
        try:
            os.kill(pid, 0)
        except (ProcessLookupError, PermissionError):
            return True
        time.sleep(0.1)
    with contextlib.suppress(ProcessLookupError, PermissionError):
        os.kill(pid, signal.SIGKILL)
    return True


# ---------------------------------------------------------------------------
# 9. Signal handlers
# ---------------------------------------------------------------------------


def install_signal_handlers() -> None:
    """Translate SIGTERM/SIGINT into ``KeyboardInterrupt`` so the loop unwinds.

    The flock is released by the kernel when the process exits, so
    clean shutdown is not strictly required — but turning a kill into
    an exception lets the loop flush a final log line.
    """

    def _raise(_sig: int, _frame: object) -> None:
        raise KeyboardInterrupt

    signal.signal(signal.SIGTERM, _raise)
    signal.signal(signal.SIGINT, _raise)


# ---------------------------------------------------------------------------
# 10. Small style helpers
# ---------------------------------------------------------------------------


def style_block(
    body: str,
    *,
    fg: str,
    bg: str,
    bold: bool = False,
    trailing: str = "  ",
) -> str:
    """Wrap ``body`` in a tmux ``#[...]`` style block, with reset + spacer.

    The ``trailing`` default of two spaces matches the inter-segment
    gap used by the right-side renderers (``status-ai``,
    ``status-hostname``). Callers can pass ``trailing=""`` if they are
    composing this into a multi-glyph segment that places its own
    separators.
    """
    attrs = f"fg={fg},bg={bg}"
    if bold:
        attrs += ",bold"
    return f"#[{attrs}]{body}#[default]{trailing}"


def as_int(value: object) -> int:
    """Coerce a json-loaded value to int, returning 0 for anything weird.

    State snapshots are JSON, which means every scalar comes back as
    ``object`` for the typechecker. Centralising the coercion means
    every renderer's ``state.get(...)`` path is uniformly defensive
    without having to repeat the ``isinstance`` ladder five times.
    """
    if isinstance(value, bool):
        return int(value)
    if isinstance(value, (int, float)):
        return int(value)
    if isinstance(value, str):
        try:
            return int(value)
        except ValueError:
            return 0
    return 0


# ---------------------------------------------------------------------------
# 11. SQLite helpers (shared by sqlite-backed pollers)
# ---------------------------------------------------------------------------


def sqlite_connect(path: Path | str) -> sqlite3.Connection:
    """Open ``path`` in WAL mode with the poller-standard PRAGMA set.

    Centralised so every sqlite-backed poller opens its DB with the
    same settings: WAL journal mode (readers don't block writers),
    ``synchronous = NORMAL`` (the WAL sweet spot — a power loss
    can lose the last commit or two, which is fine for a derived
    cache that the next poll rebuilds), and foreign keys enabled
    (so ``ON DELETE CASCADE`` actually fires).

    ``isolation_level=None`` puts the connection in autocommit mode;
    explicit ``BEGIN ... COMMIT`` blocks (e.g. ``with conn:``) are
    the only thing that opens a write transaction. Important for
    long-lived loops that do many tiny upserts: without this we'd
    accidentally open a write txn on the first SELECT and hold it
    until the next COMMIT, blocking concurrent readers (e.g.
    ``status-gchat-poller --show`` from the renderer).

    ``check_same_thread=False`` allows passing the connection across
    threads. Neither poller does today, but the renderer's
    ``LazySpawner.read_state`` path uses sqlite for read-only audits
    and historically tripped this when the test suite did a thread
    pool exercise.
    """
    conn = sqlite3.connect(str(path), isolation_level=None, check_same_thread=False)
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode = WAL")
    conn.execute("PRAGMA synchronous = NORMAL")
    conn.execute("PRAGMA foreign_keys = ON")
    return conn


def sqlite_meta_get(conn: sqlite3.Connection, key: str) -> str | None:
    """Read one row from the conventional ``meta(k, v)`` kv table.

    Both pollers carry a small key/value side table they call ``meta``
    for their own internal bookkeeping (last-slow-call timestamps,
    self-display-name caches, etc.). Centralising the get/set pair
    means the schema convention is documented in one place and
    callers don't reinvent the upsert SQL each time.

    Returns ``None`` when the key is absent. Returning a string means
    callers always need to coerce; that's the same shape both pollers
    already used and keeps the kv table schema-free.
    """
    row = conn.execute("SELECT v FROM meta WHERE k = ?", (key,)).fetchone()
    return None if row is None else str(row["v"])


def sqlite_meta_set(conn: sqlite3.Connection, key: str, value: str) -> None:
    """Upsert into the conventional ``meta(k, v)`` kv table.

    Pair to :func:`sqlite_meta_get`. The table itself is created by
    each poller's own ``_init_schema`` because the rest of the
    schema is source-specific; only the ``meta`` convention itself
    is shared.
    """
    conn.execute(
        "INSERT INTO meta (k, v) VALUES (?, ?) "
        "ON CONFLICT(k) DO UPDATE SET v = excluded.v",
        (key, value),
    )
