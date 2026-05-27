from __future__ import annotations

import logging
from pathlib import Path

LOGGER = logging.getLogger("dotfiles-sync")

FEDORA_SYSTEMD_MASKS: tuple[str, ...] = (
    "mako.service",
    "waybar.service",
    "kanshi.service",
    "foot-server.service",
    "foot-server.socket",
)


def _mask_path(target: Path, unit: str) -> Path:
    return target / ".config" / "systemd" / "user" / unit


def apply_fedora_systemd_masks(target: Path, *, verbose: bool) -> None:
    """Mask vendor user units that compete with sway-session-owned units."""

    user_dir = target / ".config" / "systemd" / "user"
    user_dir.mkdir(parents=True, exist_ok=True)
    for unit in FEDORA_SYSTEMD_MASKS:
        path = _mask_path(target, unit)
        if path.is_symlink() and path.readlink() == Path("/dev/null"):
            if verbose:
                LOGGER.info(f"MASK OK: {path} -> /dev/null")
            continue
        if path.exists() or path.is_symlink():
            path.unlink()
        path.symlink_to("/dev/null")
        LOGGER.warning(f"MASK: {path} -> /dev/null")


def check_fedora_systemd_masks(
    target: Path, *, verbose: bool, ignore: set[str]
) -> bool:
    """Return True when expected vendor unit masks are missing or wrong."""

    has_issues = False
    for unit in FEDORA_SYSTEMD_MASKS:
        issue_id = f"fedora-systemd-mask:{unit}"
        if issue_id in ignore:
            continue
        path = _mask_path(target, unit)
        ok = path.is_symlink() and path.readlink() == Path("/dev/null")
        if ok:
            if verbose:
                LOGGER.debug(f"MASK OK: {path} -> /dev/null")
            continue
        has_issues = True
        if path.is_symlink():
            actual = f"symlink to {path.readlink()}"
        elif path.exists():
            actual = "exists but is not a symlink"
        else:
            actual = "missing"
        LOGGER.warning(
            f"Missing vendor unit mask: {path} should point to /dev/null ({actual})"
        )
        LOGGER.warning(f"  (--ignore {issue_id})")
    return has_issues
