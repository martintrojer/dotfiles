from __future__ import annotations

import logging
import os
import platform
import re
import shutil
import subprocess
from collections.abc import Callable
from pathlib import Path

from .config import REPO_ROOT, lazy_header
from .external import _pinned_clone_head, _pinned_clone_resolve
from .pins import (
    MU_CREW_DOTFILES,
    MU_CREW_DOTFILES_DEST,
    TPM,
    TPM_DEST,
    ZSH_PLUGINS,
    ZSH_PLUGINS_DEST,
)

LOGGER = logging.getLogger("dotfiles-sync")


def _check_pinned_clone(
    dest: Path,
    *,
    name: str,
    ref: str,
    issue_id: str,
    print_header: Callable[[], None],
    verbose: bool,
) -> bool:
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


def check_zsh_plugins(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    plugins_dir = target / ZSH_PLUGINS_DEST
    print_header = lazy_header("zsh-plugins")
    found_issue = False
    for name, _url, ref in ZSH_PLUGINS:
        issue_id = f"zsh-plugin:{name}"
        if issue_id in ignore:
            continue
        found_issue |= _check_pinned_clone(
            plugins_dir / name,
            name=name,
            ref=ref,
            issue_id=issue_id,
            print_header=print_header,
            verbose=verbose,
        )
    return found_issue


def check_tmux_tpm(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    name, _url, ref = TPM
    issue_id = f"tmux-tpm:{name}"
    if issue_id in ignore:
        return False
    return _check_pinned_clone(
        target / TPM_DEST,
        name=name,
        ref=ref,
        issue_id=issue_id,
        print_header=lazy_header("tmux-tpm"),
        verbose=verbose,
    )


TMUX_CONF: Path = REPO_ROOT / "tmux" / ".tmux.conf"
_TPM_PLUGIN_RE = re.compile(
    r"""^\s*set(?:-option)?\s+-g\s+@plugin\s+['"]([^'"]+)['"]"""
)


def tpm_plugins(conf: Path) -> list[str]:
    """Return the ``owner/repo`` of every ``set -g @plugin`` line in ``conf``."""
    return [
        m.group(1)
        for line in conf.read_text().splitlines()
        if (m := _TPM_PLUGIN_RE.match(line))
    ]


def check_tpm_plugins(
    target: Path, *, verbose: bool, ignore: set[str], conf: Path = TMUX_CONF
) -> bool:
    """Every ``@plugin`` has a checkout, and no checkout is left without one.

    dotfiles-sync pins TPM itself; TPM owns the plugins. But nothing reruns
    TPM's install when a new ``@plugin`` line arrives through a sync, and
    tmux says nothing about a plugin that is not there: its keys and options
    just never appear. Removed plugins linger the other way, still on disk.
    TPM clones each plugin to ``~/.tmux/plugins/<repo basename>``.
    """
    plugins_dir = target / TPM_DEST.parent
    print_header = lazy_header("tmux-plugins")
    wanted = {spec.rsplit("/", 1)[-1]: spec for spec in tpm_plugins(conf)}
    found_issue = False
    for name, spec in sorted(wanted.items()):
        issue_id = f"tmux-plugin:{name}"
        if issue_id in ignore:
            continue
        if not (plugins_dir / name).is_dir():
            print_header()
            LOGGER.warning(
                f"MISSING: {spec} (run {plugins_dir / 'tpm/bin/install_plugins'}"
                f"; --ignore {issue_id})"
            )
            found_issue = True
        elif verbose:
            LOGGER.debug(f"OK: {spec}")
    if plugins_dir.is_dir():
        for child in sorted(plugins_dir.iterdir()):
            issue_id = f"tmux-plugin:{child.name}"
            if not child.is_dir() or child.name in wanted or issue_id in ignore:
                continue
            print_header()
            LOGGER.warning(
                f"STALE: {child} has no @plugin line (run "
                f"{plugins_dir / 'tpm/bin/clean_plugins'}; --ignore {issue_id})"
            )
            found_issue = True
    return found_issue


def _murmur_state_dir(target: Path) -> Path:
    """Mirror murmur's own resolution order: explicit, then XDG, then default."""
    explicit = os.environ.get("MURMUR_STATE_DIR")
    if explicit:
        return Path(explicit)
    xdg = os.environ.get("XDG_STATE_HOME")
    if xdg:
        return Path(xdg) / "murmur"
    return target / ".local" / "state" / "murmur"


def check_mu_crew_dotfiles(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    name, _url, ref = MU_CREW_DOTFILES
    issue_id = f"mu-crew-dotfiles:{name}"
    if issue_id in ignore:
        return False
    return _check_pinned_clone(
        target / MU_CREW_DOTFILES_DEST,
        name=name,
        ref=ref,
        issue_id=issue_id,
        print_header=lazy_header("mu-crew-dotfiles"),
        verbose=verbose,
    )


def _tmux_server_path() -> str | None:
    """The PATH the running tmux server hands to its own `run-shell` children.

    A tmux server inherits PATH from whatever shell started it and keeps it for
    its whole life, so it can lag behind the interactive PATH by a login: a
    freshly `npm i -g`'d murmur resolves in your terminal while the focus hooks
    and the `prefix + a` popup still see nothing. Returns None when there is no
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

    The tmux package uses it for agent state through mu-crew/dotfiles: murmur
    publishes the @murmur_* options the glyphs and pill read, `prefix + a` runs
    `murmur pick`, and three focus hooks call `murmur clear`. A missing binary
    leaves the pill empty, while the picker reports that murmur is unavailable
    and the focus hooks safely no-op.

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
            f"(npm i -g @mu-crew/murmur, then murmur init) (--ignore {issue_id})"
        )
        return True

    found_issue = False

    if not reachable_from_tmux:
        print_header()
        LOGGER.warning(
            f"UNREACHABLE: murmur is on your PATH but not the tmux server's; "
            f"status and focus hooks no-op; picker reports it unavailable "
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


def check_brew_python(
    *, verbose: bool, ignore: set[str], os_name: str | None = None
) -> bool:
    """On macOS, `python3` must be Homebrew's, not Apple's 3.9.

    The linked scripts (dotfiles-sync, tsesh, museum-search) run through
    `#!/usr/bin/env python3` and need 3.11+ for tomllib. Homebrew drops the
    unversioned `python3` link when its `python@3` alias moves to a new minor
    and the old keg stays installed, so a routine `brew upgrade` quietly hands
    the name back to /usr/bin/python3. Checked on the tmux server's PATH too,
    because tsesh runs from tmux bindings.
    """
    issue_id = "brew-python"
    if issue_id in ignore or (os_name or platform.system()) != "Darwin":
        return False
    print_header = lazy_header("brew-python")

    brew = shutil.which("brew")
    if brew is None:
        print_header()
        LOGGER.warning(f"MISSING: brew is not on PATH (--ignore {issue_id})")
        return True
    prefix = subprocess.run(
        [brew, "--prefix"], capture_output=True, text=True, check=False
    ).stdout.strip()
    want = Path(prefix) / "bin" / "python3"
    if not want.exists():
        print_header()
        LOGGER.warning(
            f"MISSING: no Homebrew python3 at {want} (brew install python@3) "
            f"(--ignore {issue_id})"
        )
        return True

    found_issue = False
    paths: dict[str, str | None] = {"your PATH": None}
    if (tmux_path := _tmux_server_path()) is not None:
        paths["the tmux server's PATH"] = tmux_path
    for label, path in paths.items():
        found = shutil.which("python3", path=path)
        if found is None or Path(found) != want:
            print_header()
            LOGGER.warning(
                f"SHADOWED: python3 on {label} is {found}, not {want} "
                f"(--ignore {issue_id})"
            )
            found_issue = True
    if not found_issue and verbose:
        LOGGER.debug(f"OK: python3 is {want}")
    return found_issue
