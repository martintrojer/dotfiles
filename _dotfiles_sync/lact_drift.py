from __future__ import annotations

import logging
from pathlib import Path

LOGGER = logging.getLogger("dotfiles-sync")

# Repo copy is the source of truth; the live daemon owns the system path and
# rewrites it on every GUI tweak (apply_settings_timer), so we never symlink —
# we just check for drift and let the human re-snapshot deliberately.
REPO_CONFIG = Path(__file__).resolve().parent.parent / "fedora" / "data" / "lact" / "config.yaml"
SYSTEM_CONFIG = Path("/etc/lact/config.yaml")
ISSUE_ID = "lact-drift:config"


def _load_yaml(path: Path):
    import yaml

    return yaml.safe_load(path.read_text())


def check_lact_drift(*, verbose: bool, ignore: set[str]) -> bool:
    """Return True when the live LACT config differs from the repo snapshot.

    Compares semantically (parsed YAML), since the daemon reorders/reformats
    the file when it rewrites it. Missing system file (LACT not installed) is
    not an issue; a missing repo snapshot is. On a LACT host, a missing pyyaml
    is also an issue: we'd otherwise silently fail to detect drift.
    """
    if ISSUE_ID in ignore:
        return False

    if not REPO_CONFIG.is_file():
        LOGGER.warning("\n[lact-drift]")
        LOGGER.warning(
            f"MISSING-SNAPSHOT: repo copy not found at {REPO_CONFIG} "
            f"(--ignore {ISSUE_ID})"
        )
        return True

    if not SYSTEM_CONFIG.is_file():
        # LACT not installed on this host — nothing to drift against.
        if verbose:
            LOGGER.debug(f"SKIP: no system config at {SYSTEM_CONFIG}")
        return False

    # We only get here when the live config exists, i.e. this is a LACT host.
    # pyyaml is then a hard requirement: skipping silently would mask real
    # drift, so a missing dependency is itself an issue worth surfacing.
    try:
        import yaml  # noqa: F401
    except ModuleNotFoundError:
        LOGGER.warning("\n[lact-drift]")
        LOGGER.warning(
            f"MISSING-DEP: pyyaml not installed but {SYSTEM_CONFIG} exists; "
            f"cannot verify LACT config drift. Install python3-pyyaml "
            f"(--ignore {ISSUE_ID})"
        )
        return True

    try:
        repo_data = _load_yaml(REPO_CONFIG)
        system_data = _load_yaml(SYSTEM_CONFIG)
    except Exception as exc:  # OSError + yaml.YAMLError
        LOGGER.warning("\n[lact-drift]")
        LOGGER.warning(f"UNREADABLE: {exc} (--ignore {ISSUE_ID})")
        return True

    if repo_data == system_data:
        if verbose:
            LOGGER.debug(f"OK: {SYSTEM_CONFIG} matches {REPO_CONFIG}")
        return False

    LOGGER.warning("\n[lact-drift]")
    LOGGER.warning(f"DRIFT: {SYSTEM_CONFIG} differs from repo snapshot {REPO_CONFIG}")
    LOGGER.warning(f"  diff:    diff {REPO_CONFIG} {SYSTEM_CONFIG}")
    LOGGER.warning(
        f"  adopt:   sudo cp {SYSTEM_CONFIG} {REPO_CONFIG}  "
        "(snapshot live tune into the repo, then commit)"
    )
    LOGGER.warning(
        f"  restore: sudo cp {REPO_CONFIG} {SYSTEM_CONFIG} && "
        "sudo systemctl restart lactd  (push repo value back to the daemon)"
    )
    LOGGER.warning(f"  (--ignore {ISSUE_ID})")
    return True
