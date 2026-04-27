from __future__ import annotations

import logging
from pathlib import Path

from .external import _pinned_clone_head, _pinned_clone_resolve
from .pins import TPM, TPM_DEST, ZSH_PLUGINS, ZSH_PLUGINS_DEST

LOGGER = logging.getLogger("dotfiles-sync")


def check_zsh_plugins(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    plugins_dir = target / ZSH_PLUGINS_DEST
    found_issue = False
    for name, _url, ref in ZSH_PLUGINS:
        issue_id = f"zsh-plugin:{name}"
        if issue_id in ignore:
            continue
        dest = plugins_dir / name
        if not dest.is_dir():
            if not found_issue:
                LOGGER.warning("\n[zsh-plugins]")
            LOGGER.warning(f"MISSING: {name} (--ignore {issue_id})")
            found_issue = True
            continue
        target_sha = _pinned_clone_resolve(dest, ref)
        current_sha = _pinned_clone_head(dest)
        if target_sha is None:
            if not found_issue:
                LOGGER.warning("\n[zsh-plugins]")
            LOGGER.warning(
                f"UNKNOWN-REF: {name} ({ref} not in local clone; --ignore {issue_id})"
            )
            found_issue = True
            continue
        if current_sha != target_sha:
            if not found_issue:
                LOGGER.warning("\n[zsh-plugins]")
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
    if not dest.is_dir():
        LOGGER.warning("\n[tmux-tpm]")
        LOGGER.warning(f"MISSING: {name} (--ignore {issue_id})")
        return True
    target_sha = _pinned_clone_resolve(dest, ref)
    current_sha = _pinned_clone_head(dest)
    if target_sha is None:
        LOGGER.warning("\n[tmux-tpm]")
        LOGGER.warning(
            f"UNKNOWN-REF: {name} ({ref} not in local clone; --ignore {issue_id})"
        )
        return True
    if current_sha != target_sha:
        LOGGER.warning("\n[tmux-tpm]")
        LOGGER.warning(
            f"DRIFT: {name} HEAD={current_sha[:12] if current_sha else '?'} "
            f"want={target_sha[:12]} (--ignore {issue_id})"
        )
        return True
    if verbose:
        LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
    return False


def check_agent_notify(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    issue_id = "agent-notify:codex"
    if issue_id in ignore:
        return False
    path = target / ".codex" / "config.toml"
    if not path.is_file():
        LOGGER.warning("\n[agent-notify]")
        LOGGER.warning(
            f"MISSING: codex config not found at {path} (--ignore {issue_id})"
        )
        return True
    try:
        content = path.read_text()
    except OSError as exc:
        LOGGER.warning("\n[agent-notify]")
        LOGGER.warning(
            f"UNREADABLE: codex config at {path}: {exc} (--ignore {issue_id})"
        )
        return True
    if "agent-attention" not in content:
        LOGGER.warning("\n[agent-notify]")
        LOGGER.warning(
            f"NO-NOTIFY: codex config at {path} doesn't mention `agent-attention` (--ignore {issue_id})"
        )
        return True
    if verbose:
        LOGGER.debug(f"OK: codex notify hook present in {path}")
    return False
