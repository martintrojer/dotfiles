from __future__ import annotations

import logging
import platform
from pathlib import Path

from .model import PackageScope, SystemInfo

LOGGER = logging.getLogger("dotfiles-sync")


def detect_system() -> SystemInfo:
    os_name = platform.system()
    distro_id = ""

    if os_name == "Linux":
        os_release = Path("/etc/os-release")
        if os_release.is_file():
            for line in os_release.read_text().splitlines():
                if line.startswith("ID="):
                    distro_id = line.split("=", 1)[1].strip().strip('"')
                    break

    if distro_id:
        LOGGER.info(f"Detected distro: {distro_id}")

    return SystemInfo(
        os_name=os_name,
        is_fedora=distro_id.lower() == "fedora",
    )


def active_scopes(system: SystemInfo) -> set[PackageScope]:
    """Canonical scope selection — all activation rules live here."""
    scopes: set[PackageScope] = {"common"}
    if system.os_name == "Darwin":
        scopes.add("darwin")
    elif system.is_fedora:
        scopes.add("linux")
        scopes.add("fedora")
    return scopes
