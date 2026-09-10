from __future__ import annotations

import ipaddress
import logging
import re
from pathlib import Path
from urllib.parse import urlparse

from .config import lazy_header
from .repo_checks import iter_repo_files, repo_rel

LOGGER = logging.getLogger("dotfiles-sync")

PRIVATE_ENV_FILENAME_MARKERS = (
    "api-key",
    "credential",
    "credentials",
    "local-env",
    "private-env",
    "secret",
    "secrets",
    "token",
    "tokens",
)
PRIVATE_ENV_SAFE_FILENAMES = {
    ".env.example",
    ".env.sample",
    ".env.template",
    "credentials.example",
    "credentials.sample",
    "secrets.example",
    "secrets.sample",
}
ENV_ASSIGNMENT_SUFFIXES = {
    "",
    ".bash",
    ".conf",
    ".env",
    ".fish",
    ".ini",
    ".json",
    ".jsonc",
    ".ksh",
    ".profile",
    ".properties",
    ".sh",
    ".toml",
    ".yaml",
    ".yml",
    ".zsh",
}
ENV_ASSIGNMENT_FILENAMES = {
    ".bash_profile",
    ".bashrc",
    ".envrc",
    ".profile",
    ".zprofile",
    ".zshenv",
    ".zshrc",
}
SECRET_ENV_ASSIGNMENT_RE = re.compile(
    r"^\s*(?:export\s+)?"
    r"(?P<name>[A-Z][A-Z0-9_]*"
    r"(?:API_KEY|TOKEN|SECRET|PASSWORD|PRIVATE_KEY|ACCESS_KEY|CLIENT_SECRET)"
    r"[A-Z0-9_]*)\s*=\s*(?P<value>.+?)\s*$"
)
PRIVATE_ENDPOINT_ASSIGNMENT_RE = re.compile(
    r"^\s*(?:export\s+)?"
    r"(?P<name>[A-Z][A-Z0-9_]*(?:BASE_URL|API_URL|ENDPOINT|URL)[A-Z0-9_]*)"
    r"\s*=\s*(?P<value>.+?)\s*$"
)


def _looks_like_private_env_file(path: Path) -> bool:
    name = path.name.lower()
    if name in PRIVATE_ENV_SAFE_FILENAMES:
        return False
    if name == ".env" or name.startswith(".env."):
        return True
    return any(marker in name for marker in PRIVATE_ENV_FILENAME_MARKERS)


def _should_scan_env_assignments(path: Path) -> bool:
    return (
        path.name in ENV_ASSIGNMENT_FILENAMES or path.suffix in ENV_ASSIGNMENT_SUFFIXES
    )


def _assignment_value(raw_value: str) -> str:
    value = raw_value.strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in {'"', "'"}:
        value = value[1:-1].strip()
    return value


def _is_placeholder_or_reference(value: str) -> bool:
    lowered = value.lower()
    if not value or value.startswith(("$", "`")):
        return True
    if "..." in value or value in {"…", "<redacted>", "<secret>"}:
        return True
    return any(
        marker in lowered
        for marker in (
            "change_me",
            "changeme",
            "example",
            "placeholder",
            "redacted",
            "replace_me",
            "your_",
        )
    )


def _is_private_endpoint(value: str) -> bool:
    if _is_placeholder_or_reference(value):
        return False

    parsed = urlparse(value if "://" in value else f"//{value}")
    host = parsed.hostname
    if host is None:
        return False

    lowered = host.lower()
    if lowered == "localhost":
        return False
    if lowered == "local":
        return True
    if lowered.endswith((".internal", ".lan", ".local")):
        return True

    try:
        ip = ipaddress.ip_address(lowered)
    except ValueError:
        return False
    # Loopback is the same address on every machine, so a service bound to it
    # is a property of the config, not of this host. Only LAN-range addresses
    # (192.168.x, 10.x, ...) actually leak where the repo was checked out.
    return ip.is_private and not ip.is_loopback


def check_private_env_mistakes(*, ignore: set[str]) -> bool:
    """Catch local env/secrets files accidentally created inside the repo."""
    print_header = lazy_header("private-env")
    found_issue = False

    def warn(message: str) -> None:
        nonlocal found_issue
        print_header()
        found_issue = True
        LOGGER.warning(message)

    for path in iter_repo_files():
        rel_path = repo_rel(path)
        if _looks_like_private_env_file(path):
            issue_id = f"private-file:{rel_path}"
            if issue_id not in ignore:
                warn(
                    f"PRIVATE-FILE: {rel_path} looks local/private; keep it under "
                    f"$HOME, not the repo  (--ignore {issue_id})"
                )

        if not _should_scan_env_assignments(path):
            continue

        try:
            lines = path.read_text(errors="replace").splitlines()
        except OSError:
            continue

        for lineno, line in enumerate(lines, start=1):
            secret_match = SECRET_ENV_ASSIGNMENT_RE.match(line)
            if secret_match is not None:
                name = secret_match.group("name")
                value = _assignment_value(secret_match.group("value"))
                issue_id = f"private-env:{rel_path}:{name}"
                if issue_id not in ignore and not _is_placeholder_or_reference(value):
                    warn(
                        f"PRIVATE-ENV: {rel_path}:{lineno} assigns {name}; move "
                        f"machine-local secrets to ~/.zsh/zz-local-env.zsh  "
                        f"(--ignore {issue_id})"
                    )

            endpoint_match = PRIVATE_ENDPOINT_ASSIGNMENT_RE.match(line)
            if endpoint_match is not None:
                name = endpoint_match.group("name")
                value = _assignment_value(endpoint_match.group("value"))
                issue_id = f"private-endpoint:{rel_path}:{name}"
                if issue_id not in ignore and _is_private_endpoint(value):
                    warn(
                        f"PRIVATE-ENDPOINT: {rel_path}:{lineno} assigns private "
                        f"endpoint {name}; keep machine-local endpoints outside "
                        f"the repo  (--ignore {issue_id})"
                    )

    return found_issue
