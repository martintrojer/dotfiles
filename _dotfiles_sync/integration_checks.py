from __future__ import annotations

import logging
import os
import shutil
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


def check_murmur(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    """Verify murmur is installed, initialised, and linked into pi.

    The tmux package hard-depends on it: `status-ai` shells out to `murmur
    status`, `prefix + a` runs `murmur pick`, and three focus hooks call
    `murmur clear`. Those all fail quietly -- a missing binary means an empty
    status segment and a popup that flashes and closes, which reads as "no
    agents running" rather than "the tool is gone".

    murmur is an npm package, not a symlink, so `--apply` cannot install it and
    this check cannot repair anything. It only tells you which of the three
    steps is missing.
    """
    issue_id = "murmur"
    if issue_id in ignore:
        return False
    print_header = lazy_header("murmur")

    if shutil.which("murmur") is None:
        print_header()
        LOGGER.warning(
            f"MISSING: murmur is not on PATH; tmux agent state is dead "
            f"(npm i -g @martintrojer/murmur, then murmur init) (--ignore {issue_id})"
        )
        return True

    found_issue = False

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
        LOGGER.debug("OK: murmur installed, initialised, linked into pi")
    return found_issue
