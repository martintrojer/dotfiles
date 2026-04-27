from __future__ import annotations

import logging
import os
import subprocess
from pathlib import Path

from .config import SCRIPT_DIR
from .pins import TPM, TPM_DEST, ZSH_PLUGINS, ZSH_PLUGINS_DEST

LOGGER = logging.getLogger("dotfiles-sync")


# Shared by zsh plugin pinning and TPM pinning.
def _pinned_clone_head(dest: Path) -> str | None:
    """Return the resolved object SHA at HEAD, or None if not a git repo."""
    result = subprocess.run(
        ["git", "-C", str(dest), "rev-parse", "--verify", "HEAD"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def _pinned_clone_resolve(dest: Path, ref: str) -> str | None:
    """Resolve the SHA of a ref inside an existing clone, or None if missing."""
    result = subprocess.run(
        ["git", "-C", str(dest), "rev-parse", "--verify", f"{ref}^{{commit}}"],
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def apply_zsh_plugins(target: Path, *, verbose: bool) -> None:
    """Clone or update zsh plugins listed in ZSH_PLUGINS to the pinned refs."""
    plugins_dir = target / ZSH_PLUGINS_DEST
    plugins_dir.mkdir(parents=True, exist_ok=True)
    header_printed = False

    def _print_header() -> None:
        nonlocal header_printed
        if not header_printed:
            LOGGER.warning("\n[zsh-plugins]")
            header_printed = True

    for name, url, ref in ZSH_PLUGINS:
        dest = plugins_dir / name
        if not dest.exists():
            _print_header()
            LOGGER.warning(f"CLONING: {name} @ {ref}")
            subprocess.run(
                ["git", "clone", "--quiet", url, str(dest)],
                check=True,
            )
        else:
            target_sha = _pinned_clone_resolve(dest, ref)
            if target_sha is None:
                _print_header()
                LOGGER.warning(f"FETCHING: {name} (ref {ref} not in clone)")
                subprocess.run(
                    ["git", "-C", str(dest), "fetch", "--tags", "--quiet"],
                    check=True,
                )
                target_sha = _pinned_clone_resolve(dest, ref)
            current_sha = _pinned_clone_head(dest)
            if target_sha is not None and current_sha == target_sha:
                if verbose:
                    LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
                continue
        subprocess.run(
            ["git", "-C", str(dest), "checkout", "--quiet", ref],
            check=True,
        )
        sha = _pinned_clone_head(dest) or "unknown"
        _print_header()
        LOGGER.warning(f"PINNED: {name} @ {ref} ({sha[:12]})")


def apply_tmux_tpm(target: Path, *, verbose: bool) -> None:
    """Clone or fast-forward TPM to its pinned ref under ~/.tmux/plugins/tpm."""
    name, url, ref = TPM
    dest = target / TPM_DEST
    dest.parent.mkdir(parents=True, exist_ok=True)
    header_printed = False

    def _print_header() -> None:
        nonlocal header_printed
        if not header_printed:
            LOGGER.warning("\n[tmux-tpm]")
            header_printed = True

    if not dest.exists():
        _print_header()
        LOGGER.warning(f"CLONING: {name} @ {ref}")
        subprocess.run(["git", "clone", "--quiet", url, str(dest)], check=True)
    else:
        target_sha = _pinned_clone_resolve(dest, ref)
        if target_sha is None:
            _print_header()
            LOGGER.warning(f"FETCHING: {name} (ref {ref} not in clone)")
            subprocess.run(
                ["git", "-C", str(dest), "fetch", "--tags", "--quiet"],
                check=True,
            )
            target_sha = _pinned_clone_resolve(dest, ref)
        current_sha = _pinned_clone_head(dest)
        if target_sha is not None and current_sha == target_sha:
            if verbose:
                LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
            return
    subprocess.run(
        ["git", "-C", str(dest), "checkout", "--quiet", ref],
        check=True,
    )
    sha = _pinned_clone_head(dest) or "unknown"
    _print_header()
    LOGGER.warning(f"PINNED: {name} @ {ref} ({sha[:12]})")


def _agent_link_apply(
    *,
    label: str,
    src_dir: Path,
    dest_dir: Path,
    expected_names: set[str],
    verbose: bool,
) -> None:
    """Symlink each name in expected_names from src_dir into dest_dir."""
    dest_dir.mkdir(parents=True, exist_ok=True)
    header_printed = False

    def _print_header() -> None:
        nonlocal header_printed
        if not header_printed:
            LOGGER.warning(f"\n[{label}]")
            header_printed = True

    for name in sorted(expected_names):
        source = src_dir / name
        dest = dest_dir / name
        relative = os.path.relpath(source, dest.parent)
        if dest.is_symlink():
            if os.readlink(dest) == relative:
                if verbose:
                    LOGGER.debug(f"OK: {dest} -> {relative}")
                continue
            dest.unlink()
        elif dest.exists():
            _print_header()
            LOGGER.warning(
                f"BLOCKED: {dest} exists and is not a symlink; leaving alone"
            )
            continue
        dest.symlink_to(relative)
        _print_header()
        LOGGER.warning(f"LINKED: {dest.name}")

    if not dest_dir.is_dir():
        return
    for entry in sorted(dest_dir.iterdir()):
        if not entry.is_symlink() or entry.name in expected_names:
            continue
        target_str = os.readlink(entry)
        target_path = (dest_dir / target_str).resolve()
        try:
            target_path.relative_to(src_dir.resolve())
        except ValueError:
            continue
        entry.unlink()
        _print_header()
        LOGGER.warning(f"PRUNED: {entry.name} (no source in {src_dir.name}/)")


def apply_skills_symlinks(target: Path, *, verbose: bool) -> None:
    src_dir = SCRIPT_DIR / "skills"
    if not src_dir.is_dir():
        return
    expected_names = {
        path.name
        for path in src_dir.iterdir()
        if path.is_dir() and not path.name.startswith(".")
    }
    _agent_link_apply(
        label="agents-skills",
        src_dir=src_dir,
        dest_dir=target / ".agents" / "skills",
        expected_names=expected_names,
        verbose=verbose,
    )


def apply_pi_extensions_symlinks(target: Path, *, verbose: bool) -> None:
    src_dir = SCRIPT_DIR / "pi" / "extensions"
    if not src_dir.is_dir():
        return
    expected_names = {
        path.name
        for path in src_dir.iterdir()
        if path.is_file() and path.suffix == ".ts"
    }
    _agent_link_apply(
        label="pi-extensions",
        src_dir=src_dir,
        dest_dir=target / ".pi" / "agent" / "extensions",
        expected_names=expected_names,
        verbose=verbose,
    )
