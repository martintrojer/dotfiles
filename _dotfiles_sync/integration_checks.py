from __future__ import annotations

import logging
import os
import shutil
import subprocess
from pathlib import Path

from .config import lazy_header
from .external import _pinned_clone_head, _pinned_clone_resolve
from .pins import TPM, TPM_DEST, ZSH_PLUGINS, ZSH_PLUGINS_DEST

LOGGER = logging.getLogger("dotfiles-sync")


def check_zsh_plugins(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    plugins_dir = target / ZSH_PLUGINS_DEST
    print_header = lazy_header("zsh-plugins")
    found_issue = False
    for name, _url, ref in ZSH_PLUGINS:
        issue_id = f"zsh-plugin:{name}"
        if issue_id in ignore:
            continue
        dest = plugins_dir / name
        if not dest.is_dir():
            print_header()
            LOGGER.warning(f"MISSING: {name} (--ignore {issue_id})")
            found_issue = True
            continue
        target_sha = _pinned_clone_resolve(dest, ref)
        current_sha = _pinned_clone_head(dest)
        if target_sha is None:
            print_header()
            LOGGER.warning(
                f"UNKNOWN-REF: {name} ({ref} not in local clone; --ignore {issue_id})"
            )
            found_issue = True
            continue
        if current_sha != target_sha:
            print_header()
            LOGGER.warning(
                f"DRIFT: {name} HEAD={current_sha[:12] if current_sha else '?'} "
                f"want={target_sha[:12]} (--ignore {issue_id})"
            )
            found_issue = True
        elif verbose:
            LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
    return found_issue


def check_tmux_tpm(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    name, _url, ref = TPM
    issue_id = f"tmux-tpm:{name}"
    if issue_id in ignore:
        return False
    dest = target / TPM_DEST
    print_header = lazy_header("tmux-tpm")
    if not dest.is_dir():
        print_header()
        LOGGER.warning(f"MISSING: {name} (--ignore {issue_id})")
        return True
    target_sha = _pinned_clone_resolve(dest, ref)
    current_sha = _pinned_clone_head(dest)
    if target_sha is None:
        print_header()
        LOGGER.warning(
            f"UNKNOWN-REF: {name} ({ref} not in local clone; --ignore {issue_id})"
        )
        return True
    if current_sha != target_sha:
        print_header()
        LOGGER.warning(
            f"DRIFT: {name} HEAD={current_sha[:12] if current_sha else '?'} "
            f"want={target_sha[:12]} (--ignore {issue_id})"
        )
        return True
    if verbose:
        LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
    return False


def _murmur_state_dir(target: Path) -> Path:
    """Mirror murmur's own resolution order: explicit, then XDG, then default."""
    explicit = os.environ.get("MURMUR_STATE_DIR")
    if explicit:
        return Path(explicit)
    xdg = os.environ.get("XDG_STATE_HOME")
    if xdg:
        return Path(xdg) / "murmur"
    return target / ".local" / "state" / "murmur"


def _tmux_server_path() -> str | None:
    """The PATH the running tmux server hands to its own `run-shell` children.

    A tmux server inherits PATH from whatever shell started it and keeps it for
    its whole life, so it can lag behind the interactive PATH by a login: a
    freshly `npm i -g`'d murmur resolves in your terminal while `status-ai` and
    the `prefix + a` popup still see nothing. Returns None when there is no
    server to ask, in which case the caller's PATH is the only answer available.
    """
    tmux = shutil.which("tmux")
    if tmux is None:
        return None
    result = subprocess.run(
        [tmux, "show-environment", "-g", "PATH"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    line = result.stdout.strip()
    # `-PATH` means "unset in the global environment", not "empty".
    if not line.startswith("PATH="):
        return None
    return line.removeprefix("PATH=") or None


def check_murmur(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    """Verify murmur is installed, initialised, and linked into pi.

    The tmux package hard-depends on it: `status-ai` shells out to `murmur
    status`, `prefix + a` runs `murmur pick`, and three focus hooks call
    `murmur clear`. Those all fail quietly -- a missing binary means an empty
    status segment and a popup that flashes and closes, which reads as "no
    agents running" rather than "the tool is gone".

    murmur is an npm package, not a symlink, so `--apply` cannot install it and
    this check cannot repair anything. It only tells you which of the steps is
    missing.

    PATH is checked twice on purpose. This process and the tmux server can
    disagree, and it is the server's view that decides whether the hooks work.
    """
    issue_id = "murmur"
    if issue_id in ignore:
        return False
    print_header = lazy_header("murmur")

    # The consumers are tmux hooks, not this process, and a tmux server keeps
    # the PATH it was started with for its whole life -- so it can lag a login
    # behind the interactive PATH. "On my PATH" is not the question that
    # matters; "on the server's PATH" is.
    tmux_path = _tmux_server_path()
    on_own_path = shutil.which("murmur") is not None
    reachable_from_tmux = (
        shutil.which("murmur", path=tmux_path) is not None
        if tmux_path is not None
        else on_own_path
    )

    if not on_own_path and not reachable_from_tmux:
        print_header()
        LOGGER.warning(
            f"MISSING: murmur is not on PATH; tmux agent state is dead "
            f"(npm i -g @martintrojer/murmur, then murmur init) (--ignore {issue_id})"
        )
        return True

    found_issue = False

    if not reachable_from_tmux:
        print_header()
        LOGGER.warning(
            f"UNREACHABLE: murmur is on your PATH but not the tmux server's; "
            f"status, picker and focus hooks all no-op "
            f'(tmux kill-server, or tmux setenv -g PATH "$PATH") '
            f"(--ignore {issue_id})"
        )
        found_issue = True

    # Identity is per machine and lives outside the repo by design, so a fresh
    # box has the binary but no node id, and every command no-ops.
    state_dir = _murmur_state_dir(target)
    if not (state_dir / "identity.json").is_file():
        print_header()
        LOGGER.warning(
            f"UNINITIALISED: no identity at {state_dir}/identity.json "
            f"(murmur init) (--ignore {issue_id})"
        )
        found_issue = True

    # `murmur link pi` writes a real file rather than a symlink, and pins an
    # absolute store path into it, so a moved or reinstalled murmur needs a
    # re-link. A stale copy silently stops recording.
    extension = target / ".pi" / "agent" / "extensions" / "murmur.ts"
    if not extension.is_file():
        print_header()
        LOGGER.warning(
            f"UNLINKED: no pi extension at {extension} "
            f"(murmur link pi) (--ignore {issue_id})"
        )
        found_issue = True

    if not found_issue and verbose:
        LOGGER.debug(
            "OK: murmur installed, initialised, linked into pi, visible to tmux"
        )
    return found_issue


def check_codex_notify(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    """Verify the codex notify hook points at something that exists.

    This check existed once, was deleted in b1d77f4 when `agent-attention` went
    away, and the README kept advertising it for two commits. It is back because
    the failure it catches is invisible by construction: a notify hook's stdout
    and stderr go nowhere, so a hook that shells out to a deleted script fails
    silently on every single notification. That is exactly what happened -- the
    line survived pointing at `agent-attention` long after the script was gone,
    and nothing said so.

    Deliberately NOT asserting that codex is configured at all. A machine
    without codex is fine, and a missing `notify` line only means no tmux
    attention for codex, which is a choice. The one thing worth failing on is a
    line that names a script that is not there.
    """
    issue_id = "codex-notify"
    if issue_id in ignore:
        return False

    path = target / ".codex" / "config.toml"
    if not path.is_file():
        # No codex on this machine. Not a problem.
        return False

    try:
        content = path.read_text()
    except OSError as exc:
        lazy_header("codex-notify")()
        LOGGER.warning(
            f"UNREADABLE: codex config at {path}: {exc} (--ignore {issue_id})"
        )
        return True

    notify_lines = [
        line for line in content.splitlines() if line.strip().startswith("notify")
    ]
    if not notify_lines:
        # No hook configured, so nothing can break. Mentioned only when asked.
        if verbose:
            LOGGER.debug(f"OK: no codex notify hook configured in {path}")
        return False

    line = notify_lines[0]
    if "agent-attention" in line:
        lazy_header("codex-notify")()
        LOGGER.warning(
            f"STALE: codex notify at {path} still calls `agent-attention`, which was "
            f"removed -- every notification fails silently. Replace it with "
            f"`murmur notify --source codex --event-type notify --title Codex` "
            f"(--ignore {issue_id})"
        )
        return True

    if "murmur" not in line:
        lazy_header("codex-notify")()
        LOGGER.warning(
            f"UNKNOWN: codex notify at {path} does not call murmur; if the command it "
            f"names is missing, notifications fail with no output (--ignore {issue_id})"
        )
        return True

    if verbose:
        LOGGER.debug(f"OK: codex notify hook calls murmur in {path}")
    return False
