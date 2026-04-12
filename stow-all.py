#!/usr/bin/env python3
from __future__ import annotations

import argparse
import filecmp
import json
import logging
import os
import platform
import re
import shutil
import subprocess
import sys
from itertools import chain
from time import monotonic
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Final, Literal, Mapping, Sequence, cast


Action = Literal["check", "apply"]
PackageScope = Literal["common", "darwin", "linux", "fedora"]
Conflict = tuple[str, str]


@dataclass(frozen=True)
class Args:
    action: Action
    force_overwrite: bool
    show_diffs: bool
    verbose: bool
    target: str


SCRIPT_PATH = Path(__file__).resolve()
SCRIPT_DIR: Final[Path] = SCRIPT_PATH.parent
LOGGER: Final[logging.Logger] = logging.getLogger("stow-all")
SHARED_SKILLS_DIR: Final[Path] = SCRIPT_DIR / "skills"
CLAUDE_PLUGIN_DIR: Final[Path] = SCRIPT_DIR / "claude" / "mtrojer-plugin"
CLAUDE_PLUGIN_SKILLS_DIR: Final[Path] = CLAUDE_PLUGIN_DIR / "skills"
CLAUDE_PLUGIN_NAME: Final[str] = "mtrojer"
CLAUDE_PLUGIN_MARKETPLACE: Final[str] = "local"
SHARED_SKILL_LINKS: Final[tuple[tuple[str, Path], ...]] = (
    ("codex", Path(".codex/skills")),
    ("agents", Path(".agents/skills")),
)

# Portable baseline shared across hosts.
COMMON_PACKAGES: Final[list[str]] = [
    "bat",
    "btop",
    "eza",
    "gdu",
    "git",
    "jj",
    "nvim",
    "ssh",
    "tmux",
    "tuicr",
    "vale",
    "yazi",
    "zsh",
]

# macOS desktop/session layer.
DARWIN_PACKAGES: Final[list[str]] = [
    "ghostty",
    "hammerspoon",
]

# Linux desktop/session layer.
LINUX_PACKAGES: Final[list[str]] = [
    "alacritty",
    "bin",
    "fuzzel",
    "kanshi",
    "mako",
    "niri",
    "swaylock",
    "waybar",
    "wallpapers",
]

# Fedora-only additions layered on top of the Linux desktop stack.
FEDORA_PACKAGES: Final[list[str]] = [
    "containers",
    "systemd",
]

IGNORED_TOPLEVEL_DIRS: Final[set[str]] = {
    "__pycache__",
    "chrome",
    "claude",
    "fedora",
    "skills",
    "vscode",
}

CONFLICT_RE: Final[re.Pattern[str]] = re.compile(
    r"^  \* cannot stow (.*) over existing target (.*) since .*"
)


@dataclass(frozen=True)
class PackageSpec:
    name: str
    stow_dir: Path
    scope: PackageScope

    @property
    def package_dir(self) -> Path:
        return self.stow_dir / self.name


@dataclass(frozen=True)
class SystemInfo:
    os_name: str
    distro_id: str
    distro_like: str
    is_fedora_family: bool
    is_debian_family: bool


def parse_args() -> Args:
    parser = argparse.ArgumentParser(
        description="Manage all relevant dotfile packages in this repository."
    )
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "-c",
        "--check",
        dest="action",
        action="store_const",
        const="check",
        help="Check for missing stows and blocking conflicts (default)",
    )
    mode.add_argument(
        "-a",
        "--apply",
        dest="action",
        action="store_const",
        const="apply",
        help="Stow everything",
    )
    parser.set_defaults(action="check")
    parser.add_argument(
        "-f",
        "--force-overwrite",
        action="store_true",
        help=(
            "Move conflicting target paths into a timestamped backup dir under "
            "the target before stowing"
        ),
    )
    parser.add_argument(
        "--show-diffs",
        action="store_true",
        help="In check mode, show diff commands for plain-file conflicts",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Show verbose stow output and detailed custom checks",
    )
    parser.add_argument(
        "-t",
        "--target",
        default=os.environ.get("TARGET", str(Path.home())),
        help="Override target directory (default: %(default)s)",
    )
    namespace = parser.parse_args()
    return Args(
        action=namespace.action,
        force_overwrite=namespace.force_overwrite,
        show_diffs=namespace.show_diffs,
        verbose=namespace.verbose,
        target=namespace.target,
    )


def detect_system() -> SystemInfo:
    os_name = platform.system()
    distro_id = ""
    distro_like = ""

    if os_name == "Linux":
        os_release = Path("/etc/os-release")
        if os_release.is_file():
            values: dict[str, str] = {}
            for line in os_release.read_text().splitlines():
                if "=" not in line:
                    continue
                key, value = line.split("=", 1)
                values[key] = value.strip().strip('"')
            distro_id = values.get("ID", "")
            distro_like = values.get("ID_LIKE", "")

    distro_id_lc = distro_id.lower()
    distro_like_lc = distro_like.lower()

    is_fedora_family = any(
        marker in distro_id_lc or marker in distro_like_lc
        for marker in ("fedora", "rhel")
    )
    is_debian_family = any(
        marker in distro_id_lc or marker in distro_like_lc
        for marker in ("debian", "ubuntu")
    )

    return SystemInfo(
        os_name=os_name,
        distro_id=distro_id,
        distro_like=distro_like,
        is_fedora_family=is_fedora_family,
        is_debian_family=is_debian_family,
    )


def configure_logging(args: Args) -> None:
    level = logging.WARNING
    if args.verbose:
        level = logging.DEBUG

    logging.basicConfig(level=level, format="%(message)s")


def build_specs() -> dict[str, PackageSpec]:
    specs: dict[str, PackageSpec] = {}
    for name in COMMON_PACKAGES:
        specs[name] = PackageSpec(name=name, stow_dir=SCRIPT_DIR, scope="common")
    for name in DARWIN_PACKAGES:
        specs[name] = PackageSpec(name=name, stow_dir=SCRIPT_DIR, scope="darwin")
    for name in LINUX_PACKAGES:
        specs[name] = PackageSpec(name=name, stow_dir=SCRIPT_DIR, scope="linux")
    fedora_dir = SCRIPT_DIR / "fedora"
    for name in FEDORA_PACKAGES:
        specs[name] = PackageSpec(name=name, stow_dir=fedora_dir, scope="fedora")
    return specs


def active_package_names(system: SystemInfo) -> tuple[list[str], list[str]]:
    # Canonical package-layer selection lives here.
    # Do not duplicate OS package activation rules in shell startup files.
    active = list(COMMON_PACKAGES)
    inactive: list[str] = []

    if system.os_name == "Darwin":
        active.extend(DARWIN_PACKAGES)
        inactive.extend(LINUX_PACKAGES)
        inactive.extend(FEDORA_PACKAGES)
    elif system.os_name == "Linux":
        active.extend(LINUX_PACKAGES)
        inactive.extend(DARWIN_PACKAGES)
        if system.is_fedora_family:
            active.extend(FEDORA_PACKAGES)
        else:
            inactive.extend(FEDORA_PACKAGES)
    else:
        inactive.extend(DARWIN_PACKAGES)
        inactive.extend(LINUX_PACKAGES)
        inactive.extend(FEDORA_PACKAGES)

    return active, inactive


def ensure_stow_available() -> None:
    if shutil.which("stow") is None:
        LOGGER.error("GNU Stow is required but not installed.")
        raise SystemExit(1)


def meaningful_output(output: str) -> str:
    lines = []
    for line in output.splitlines():
        if line == "WARNING: in simulation mode so not modifying filesystem.":
            continue
        if line.startswith("UNLINK: "):
            continue
        if line.endswith(" (reverts previous action)"):
            continue
        if not line.strip():
            continue
        lines.append(line)
    return "\n".join(lines)


def run_stow_command(
    stow_dir: Path,
    packages: Sequence[str],
    target: Path,
    *,
    simulate: bool,
    verbose: bool,
) -> subprocess.CompletedProcess[str]:
    cmd = ["stow", "--restow"]
    if simulate:
        cmd.append("--no")
    if verbose:
        cmd.append("--verbose")
    cmd.extend(["-d", str(stow_dir), "-t", str(target)])
    cmd.extend(packages)
    return subprocess.run(
        cmd,
        cwd=SCRIPT_DIR,
        capture_output=True,
        text=True,
        check=False,
    )


def parse_conflicts(output: str) -> list[Conflict]:
    conflicts: list[Conflict] = []
    seen: set[str] = set()
    for line in output.splitlines():
        match = CONFLICT_RE.match(line)
        if not match:
            continue
        source_rel, target_rel = match.groups()
        if target_rel in seen:
            continue
        conflicts.append((source_rel, target_rel))
        seen.add(target_rel)
    return conflicts


def source_path_for_conflict(source_rel: str, target: Path) -> Path:
    if source_rel.startswith("dotfiles/"):
        return target / source_rel
    return SCRIPT_DIR / source_rel


def show_conflict_diffs(output: str, target: Path) -> None:
    found = False
    for source_rel, target_rel in parse_conflicts(output):
        source_path = source_path_for_conflict(source_rel, target)
        target_path = target / target_rel
        if not source_path.is_file() or not target_path.is_file():
            continue
        if not found:
            LOGGER.warning("Diffs:")
            found = True
        if filecmp.cmp(target_path, source_path, shallow=False):
            LOGGER.warning(
                f"  {target_rel} matches repo copy; "
                "safe to replace with --apply --force-overwrite"
            )
        else:
            LOGGER.warning(
                f"  diff -u {shlex_quote(target_path)} {shlex_quote(source_path)}"
            )


def backup_conflict_path(rel_path: str, target: Path, backup_root: Path) -> None:
    target_path = target / rel_path
    if not target_path.exists() and not target_path.is_symlink():
        return
    backup_path = backup_root / rel_path
    backup_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.move(str(target_path), str(backup_path))
    LOGGER.warning(f"BACKED UP: {target_path} -> {backup_path}")


def shlex_quote(path: Path) -> str:
    return subprocess.list2cmdline([str(path)])


def check_package_coverage(specs: dict[str, PackageSpec]) -> bool:
    expected = set(specs) | IGNORED_TOPLEVEL_DIRS
    found_issue = False
    for child in sorted(SCRIPT_DIR.iterdir(), key=lambda p: p.name):
        if not child.is_dir():
            continue
        if child.name.startswith("."):
            continue
        if child.name not in expected:
            if not found_issue:
                LOGGER.warning("\n[package-coverage]")
            found_issue = True
            LOGGER.warning(f"UNCLASSIFIED: {child.name}")
    return found_issue


def collect_scan_roots(
    specs: dict[str, PackageSpec], target: Path, active_names: set[str]
) -> list[Path]:
    roots: set[Path] = set()
    for name in active_names:
        spec = specs[name]
        if not spec.package_dir.is_dir():
            continue
        for child in spec.package_dir.iterdir():
            roots.add(target / child.name)
    return sorted(roots)


def owner_for_repo_path(
    path: Path, specs: dict[str, PackageSpec]
) -> PackageSpec | None:
    for spec in specs.values():
        package_root = spec.package_dir.resolve()
        try:
            path.relative_to(package_root)
        except ValueError:
            continue
        return spec
    return None


def repo_target_of_link(path: Path, script_real: Path) -> Path | None:
    if not path.is_symlink():
        return None
    try:
        raw_target = os.readlink(path)
    except OSError:
        return None
    resolved = (path.parent / raw_target).resolve(strict=False)
    try:
        resolved.relative_to(script_real)
    except ValueError:
        return None
    return resolved


def check_repo_backlinks(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
) -> bool:
    script_real = SCRIPT_DIR.resolve()
    found_stale = False
    found_invalid = False
    has_issues = False

    for scan_root in collect_scan_roots(specs, target, active_names):
        if not scan_root.exists() and not scan_root.is_symlink():
            continue

        if scan_root.is_symlink():
            scan_iter = iter((scan_root,))
        elif scan_root.is_dir():
            scan_iter = chain((scan_root,), scan_root.rglob("*"))
        else:
            scan_iter = iter((scan_root,))

        for path in scan_iter:
            if not path.is_symlink():
                continue
            repo_target = repo_target_of_link(path, script_real)
            if repo_target is None:
                continue

            spec = owner_for_repo_path(repo_target, specs)
            if spec is None:
                continue

            rel_path = path.relative_to(target)

            if not path.exists():
                if not found_stale:
                    LOGGER.warning("\n[stale-symlinks]")
                    found_stale = True
                has_issues = True
                LOGGER.warning(f"STALE: {rel_path} -> {repo_target}")
                continue

            if spec.name not in active_names:
                if not found_invalid:
                    LOGGER.warning("\n[invalid-backlinks]")
                    found_invalid = True
                has_issues = True
                LOGGER.warning(f"INVALID: {rel_path} -> {repo_target} [{spec.scope}]")

    return has_issues


def run_check_group(
    label: str,
    stow_dir: Path,
    packages: list[str],
    target: Path,
    show_diffs: bool,
    verbose: bool,
) -> bool:
    if not packages:
        return False
    result = run_stow_command(stow_dir, packages, target, simulate=True, verbose=True)
    output = meaningful_output(result.stdout + result.stderr)
    if not output:
        if verbose:
            LOGGER.debug(f"\n[{label}]")
            LOGGER.debug("OK")
        return False
    LOGGER.warning(f"\n[{label}]")
    sys.stdout.write(f"{output}\n")
    if show_diffs:
        show_conflict_diffs(result.stdout + result.stderr, target)
    return True


def run_apply_group(
    label: str,
    stow_dir: Path,
    packages: list[str],
    target: Path,
    verbose: bool,
    force_overwrite: bool,
    backup_root: Path | None,
) -> None:
    if not packages:
        return

    if force_overwrite:
        probe = run_stow_command(
            stow_dir, packages, target, simulate=True, verbose=True
        )
        conflicts = parse_conflicts(probe.stdout + probe.stderr)
        if conflicts:
            LOGGER.warning(f"\n[{label}]")
            filtered = meaningful_output(probe.stdout + probe.stderr)
            if filtered:
                sys.stdout.write(f"{filtered}\n")
            assert backup_root is not None
            for _, target_rel in conflicts:
                backup_conflict_path(target_rel, target, backup_root)

    result = run_stow_command(
        stow_dir,
        packages,
        target,
        simulate=False,
        verbose=verbose,
    )
    if result.returncode != 0:
        sys.stdout.write(result.stdout)
        sys.stderr.write(result.stderr)
        raise SystemExit(result.returncode)


def shared_skill_entries() -> list[Path]:
    return sorted(SHARED_SKILLS_DIR.iterdir(), key=lambda path: path.name)


def ensure_dir(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True)


def describe_skill_link_state(dest_dir: Path, source: Path) -> str | None:
    link_path = dest_dir / source.name
    desired = source.resolve(strict=False)
    if not link_path.exists() and not link_path.is_symlink():
        return "missing"
    if not link_path.is_symlink():
        return f"expected symlink to {desired}, found directory or file"

    resolved = link_path.resolve(strict=False)
    if resolved != desired:
        return f"expected symlink to {desired}, found symlink to {resolved}"
    return None


def replace_path(path: Path) -> None:
    if path.is_symlink() or path.is_file():
        path.unlink()
        return
    if path.is_dir():
        shutil.rmtree(path)


def is_managed_skill_link(path: Path, source: Path) -> bool:
    return path.is_symlink() and path.resolve(strict=False) == source.resolve(
        strict=False
    )


def has_unmanaged_skill_conflict(dest_dir: Path, source: Path) -> bool:
    path = dest_dir / source.name
    if not path.exists() and not path.is_symlink():
        return False
    return not is_managed_skill_link(path, source)


def remove_dir_contents(path: Path, *, keep_names: set[str] | None = None) -> None:
    keep_names = keep_names or set()
    for child in path.iterdir():
        if child.name in keep_names:
            continue
        replace_path(child)


def check_shared_skill_link_conflicts(dest_dir: Path) -> list[str]:
    conflicts: list[str] = []
    if dest_dir.exists() and not dest_dir.is_dir():
        return [f"{dest_dir}: expected directory, found file"]
    if not dest_dir.exists():
        return conflicts

    for source in shared_skill_entries():
        if has_unmanaged_skill_conflict(dest_dir, source):
            conflicts.append(str(dest_dir / source.name))
    return conflicts


def apply_shared_skill_links(dest_dir: Path) -> list[str]:
    conflicts = check_shared_skill_link_conflicts(dest_dir)
    if conflicts:
        return conflicts

    ensure_dir(dest_dir)
    for source in shared_skill_entries():
        path = dest_dir / source.name
        if is_managed_skill_link(path, source):
            continue
        if path.exists() or path.is_symlink():
            replace_path(path)
        path.symlink_to(
            os.path.relpath(source, start=path.parent), target_is_directory=True
        )
    return []


def compare_skill_trees(
    source: Path,
    dest: Path,
) -> list[str]:
    issues: list[str] = []

    if not dest.exists():
        return [f"missing: {dest}"]
    if dest.is_symlink():
        return [f"expected real directory copy at {dest}, found symlink"]
    if not dest.is_dir():
        return [f"expected directory at {dest}, found file"]

    def walk(source_dir: Path, dest_dir: Path, *, is_root: bool = False) -> None:
        if not dest_dir.exists():
            issues.append(f"missing: {dest_dir}")
            return
        if not dest_dir.is_dir():
            issues.append(f"expected directory at {dest_dir}, found file")
            return

        source_names = {path.name for path in source_dir.iterdir()}
        dest_names = {path.name for path in dest_dir.iterdir()}
        if is_root:
            dest_names.discard(".gitignore")

        for name in sorted(source_names - dest_names):
            issues.append(f"missing: {dest_dir / name}")
        for name in sorted(dest_names - source_names):
            issues.append(f"extra: {dest_dir / name}")

        for name in sorted(source_names & dest_names):
            source_path = source_dir / name
            dest_path = dest_dir / name
            if source_path.is_dir() and dest_path.is_dir():
                walk(source_path, dest_path)
                continue
            if source_path.is_file() and dest_path.is_file():
                if not filecmp.cmp(source_path, dest_path, shallow=False):
                    issues.append(f"differs: {dest_path}")
                continue
            source_kind = "directory" if source_path.is_dir() else "file"
            dest_kind = "directory" if dest_path.is_dir() else "file"
            issues.append(
                f"type mismatch: {dest_path} (source {source_kind}, dest {dest_kind})"
            )

    walk(source, dest, is_root=True)

    return issues


def sync_skill_tree(source: Path, dest: Path) -> None:
    if dest.is_symlink():
        replace_path(dest)
    ensure_dir(dest)
    remove_dir_contents(dest, keep_names={".gitignore"})
    for source_path in source.iterdir():
        shutil.copytree(
            source_path,
            dest / source_path.name,
            copy_function=shutil.copy2,
        )


def print_claude_install_instructions() -> None:
    LOGGER.warning("Install the Claude Code plugin manually:")
    LOGGER.warning(f"  /plugin marketplace add {CLAUDE_PLUGIN_DIR}")
    LOGGER.warning(
        f"  /plugin install {CLAUDE_PLUGIN_NAME}@{CLAUDE_PLUGIN_MARKETPLACE}"
    )


def print_claude_refresh_instructions() -> None:
    LOGGER.warning("Refresh the installed Claude Code plugin snapshot:")
    LOGGER.warning(
        f"  /plugin install {CLAUDE_PLUGIN_NAME}@{CLAUDE_PLUGIN_MARKETPLACE}"
    )


def log_issue_summary(issues: Sequence[str], *, max_items: int = 3) -> None:
    for issue in issues[:max_items]:
        LOGGER.warning(f"  {issue}")
    remaining = len(issues) - max_items
    if remaining > 0:
        LOGGER.warning(f"  ... {remaining} more")


def load_json(path: Path) -> object | None:
    if not path.is_file():
        return None
    try:
        return json.loads(path.read_text())
    except (OSError, json.JSONDecodeError):
        return None


def as_mapping(value: object) -> Mapping[str, object] | None:
    if isinstance(value, dict):
        return cast(Mapping[str, object], value)
    return None


def iter_claude_installed_plugin_roots(target: Path) -> list[Path]:
    roots: set[Path] = set()
    plugins_dir = target / ".claude/plugins"

    installed = as_mapping(load_json(plugins_dir / "installed_plugins.json"))
    if installed is not None:
        plugins = as_mapping(installed.get("plugins"))
        if plugins is not None:
            entries = plugins.get(f"{CLAUDE_PLUGIN_NAME}@{CLAUDE_PLUGIN_MARKETPLACE}")
            if isinstance(entries, list):
                for entry in entries:
                    entry_map = as_mapping(entry)
                    if entry_map is None:
                        continue
                    install_path = entry_map.get("installPath")
                    if isinstance(install_path, str):
                        roots.add(Path(install_path).expanduser())

    return sorted(root.resolve(strict=False) for root in roots)


def iter_claude_marketplace_sources(target: Path) -> list[Path]:
    roots: set[Path] = set()
    plugins_dir = target / ".claude/plugins"

    marketplaces = as_mapping(load_json(plugins_dir / "known_marketplaces.json"))
    if marketplaces is not None:
        marketplace = as_mapping(marketplaces.get(CLAUDE_PLUGIN_MARKETPLACE))
        if marketplace is not None:
            install_location = marketplace.get("installLocation")
            if isinstance(install_location, str):
                roots.add(Path(install_location).expanduser())

    return sorted(root.resolve(strict=False) for root in roots)


def check_shared_skills(target: Path, *, verbose: bool) -> bool:
    has_issues = False

    LOGGER.info("\n[shared-skills]")
    for label, rel_path in SHARED_SKILL_LINKS:
        path = target / rel_path
        if path.exists() and not path.is_dir():
            has_issues = True
            detail = f" ({path})" if verbose else ""
            LOGGER.warning(f"ISSUE: {label}: expected directory, found file{detail}")
            continue
        if not path.exists():
            has_issues = True
            detail = f" ({path})" if verbose else ""
            LOGGER.warning(f"ISSUE: {label}: missing directory{detail}")
            continue

        conflicts = check_shared_skill_link_conflicts(path)
        if conflicts:
            has_issues = True
            LOGGER.warning(
                f"ISSUE: {label}: {len(conflicts)} unmanaged skill conflicts"
            )
            if verbose:
                LOGGER.debug(f"  {path}")
                log_issue_summary(conflicts, max_items=10)
            else:
                log_issue_summary(conflicts)
            continue

        issues: list[str] = []
        for source in shared_skill_entries():
            problem = describe_skill_link_state(path, source)
            if problem is None:
                continue
            issues.append(f"{source.name}: {problem}")

        if issues:
            has_issues = True
            LOGGER.warning(f"ISSUE: {label}: {len(issues)} skill links out of sync")
            if verbose:
                LOGGER.debug(f"  {path}")
                log_issue_summary(issues, max_items=10)
            else:
                log_issue_summary(issues)
        else:
            if verbose:
                LOGGER.debug(f"OK: {label} ({path})")
            else:
                LOGGER.info(f"OK: {label}")

    bundle_issues = compare_skill_trees(SHARED_SKILLS_DIR, CLAUDE_PLUGIN_SKILLS_DIR)
    if bundle_issues:
        has_issues = True
        LOGGER.warning(f"ISSUE: claude bundle: {len(bundle_issues)} differences")
        if verbose:
            LOGGER.debug(f"  {CLAUDE_PLUGIN_SKILLS_DIR}")
            log_issue_summary(bundle_issues, max_items=10)
        else:
            log_issue_summary(bundle_issues)
    else:
        if verbose:
            LOGGER.debug(f"OK: claude bundle ({CLAUDE_PLUGIN_SKILLS_DIR})")
        else:
            LOGGER.info("OK: claude bundle")

    marketplace_sources = iter_claude_marketplace_sources(target)
    if marketplace_sources:
        if verbose:
            LOGGER.debug("\n[claude-marketplace]")
            for root in marketplace_sources:
                LOGGER.debug(f"OK: source ({root})")

    plugin_roots = iter_claude_installed_plugin_roots(target)
    if not plugin_roots:
        has_issues = True
        LOGGER.warning("ISSUE: claude plugin not installed")
        print_claude_install_instructions()
        return has_issues

    LOGGER.info("[claude-plugin]")
    for root in plugin_roots:
        plugin_skills_dir = root / "skills"
        issues = compare_skill_trees(SHARED_SKILLS_DIR, plugin_skills_dir)
        if issues:
            has_issues = True
            LOGGER.warning(f"ISSUE: installed copy stale: {len(issues)} differences")
            if verbose:
                LOGGER.debug(f"  {plugin_skills_dir}")
                log_issue_summary(issues, max_items=10)
            else:
                log_issue_summary(issues)
            LOGGER.warning(f"  Run: /plugin install {CLAUDE_PLUGIN_NAME}@local")
        else:
            if verbose:
                LOGGER.debug(f"OK: installed copy ({plugin_skills_dir})")
            else:
                LOGGER.info("OK: installed copy")

    return has_issues


def apply_shared_skills(target: Path, *, verbose: bool) -> None:
    LOGGER.info("\n[shared-skills]")
    for label, rel_path in SHARED_SKILL_LINKS:
        path = target / rel_path
        conflicts = apply_shared_skill_links(path)
        if conflicts:
            LOGGER.error(f"ISSUE: {label}: {len(conflicts)} unmanaged skill conflicts")
            log_issue_summary(conflicts, max_items=10)
            LOGGER.error("Resolve or move those paths before rerunning --apply.")
            raise SystemExit(1)
        if verbose:
            LOGGER.debug(f"LINKED: {label} -> {path}")
        else:
            LOGGER.info(f"LINKED: {label}")

    sync_skill_tree(SHARED_SKILLS_DIR, CLAUDE_PLUGIN_SKILLS_DIR)
    if verbose:
        LOGGER.debug(f"SYNCED: claude-plugin bundle -> {CLAUDE_PLUGIN_SKILLS_DIR}")
    else:
        LOGGER.info("SYNCED: claude-plugin bundle")

    marketplace_sources = iter_claude_marketplace_sources(target)
    if verbose and marketplace_sources:
        LOGGER.debug("\n[claude-marketplace]")
        for root in marketplace_sources:
            LOGGER.debug(f"OK: source ({root})")

    plugin_roots = iter_claude_installed_plugin_roots(target)
    if not plugin_roots:
        LOGGER.warning("Claude Code plugin install still requires one manual step.")
        print_claude_install_instructions()
        return

    stale_installed_copies = False
    for root in plugin_roots:
        plugin_skills_dir = root / "skills"
        issues = compare_skill_trees(SHARED_SKILLS_DIR, plugin_skills_dir)
        if issues:
            stale_installed_copies = True
            LOGGER.warning(f"ISSUE: installed copy stale: {len(issues)} differences")
            if verbose:
                LOGGER.debug(f"  {plugin_skills_dir}")
                log_issue_summary(issues, max_items=10)
            else:
                log_issue_summary(issues)
        elif verbose:
            LOGGER.debug(f"OK: installed copy ({plugin_skills_dir})")
        else:
            LOGGER.info("OK: installed copy")

    if stale_installed_copies:
        print_claude_refresh_instructions()


def main() -> int:
    args = parse_args()
    configure_logging(args)
    ensure_stow_available()

    target = Path(args.target).expanduser()
    if not target.is_dir():
        LOGGER.error(f"Target directory does not exist: {target}")
        return 1

    system = detect_system()
    specs = build_specs()
    active_names_list, _inactive_names_list = active_package_names(system)
    active_names = set(active_names_list)
    backup_root = None
    if args.force_overwrite:
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        backup_root = target / ".stow-all-backups" / timestamp

    LOGGER.info(f"Using target: {target}")
    LOGGER.info(f"Detected OS: {system.os_name}")
    if system.distro_id:
        if system.distro_like:
            LOGGER.info(
                f"Detected distro: {system.distro_id} (like: {system.distro_like})"
            )
        else:
            LOGGER.info(f"Detected distro: {system.distro_id}")
    LOGGER.info(f"Mode: {args.action}")
    if backup_root is not None:
        LOGGER.info(
            "Force overwrite enabled; conflicting targets will be moved to: "
            f"{backup_root}"
        )

    has_issues = False

    has_issues |= (
        run_check_group(
            "root",
            SCRIPT_DIR,
            COMMON_PACKAGES,
            target,
            args.show_diffs,
            args.verbose,
        )
        if args.action == "check"
        else False
    )

    if args.action == "apply":
        run_apply_group(
            "root",
            SCRIPT_DIR,
            COMMON_PACKAGES,
            target,
            verbose=args.verbose,
            force_overwrite=args.force_overwrite,
            backup_root=backup_root,
        )

    if system.os_name == "Darwin":
        if args.action == "check":
            has_issues |= run_check_group(
                "darwin",
                SCRIPT_DIR,
                DARWIN_PACKAGES,
                target,
                args.show_diffs,
                args.verbose,
            )
        else:
            run_apply_group(
                "darwin",
                SCRIPT_DIR,
                DARWIN_PACKAGES,
                target,
                verbose=args.verbose,
                force_overwrite=args.force_overwrite,
                backup_root=backup_root,
            )
    elif system.os_name == "Linux":
        if args.action == "check":
            has_issues |= run_check_group(
                "linux",
                SCRIPT_DIR,
                LINUX_PACKAGES,
                target,
                args.show_diffs,
                args.verbose,
            )
        else:
            run_apply_group(
                "linux",
                SCRIPT_DIR,
                LINUX_PACKAGES,
                target,
                verbose=args.verbose,
                force_overwrite=args.force_overwrite,
                backup_root=backup_root,
            )

        if system.is_fedora_family:
            if args.action == "check":
                has_issues |= run_check_group(
                    "fedora",
                    SCRIPT_DIR / "fedora",
                    FEDORA_PACKAGES,
                    target,
                    args.show_diffs,
                    args.verbose,
                )
            else:
                run_apply_group(
                    "fedora",
                    SCRIPT_DIR / "fedora",
                    FEDORA_PACKAGES,
                    target,
                    verbose=args.verbose,
                    force_overwrite=args.force_overwrite,
                    backup_root=backup_root,
                )
        elif system.is_debian_family:
            LOGGER.info("Skipping fedora/* packages on Debian-family distro.")
        else:
            LOGGER.info("Skipping fedora/* packages on non-Fedora Linux distro.")
    else:
        LOGGER.info(
            f"Skipping OS-specific packages for unsupported OS: {system.os_name}"
        )

    has_issues |= check_package_coverage(specs)

    if args.action == "check":
        has_issues |= check_shared_skills(target, verbose=args.verbose)
        LOGGER.info("\n[repo-backlinks]")
        LOGGER.info("Scanning for stale or invalid repo backlinks...")
        backlink_scan_started = monotonic()
        has_issues |= check_repo_backlinks(target, specs, active_names)
        backlink_scan_elapsed = monotonic() - backlink_scan_started
        LOGGER.info(f"Backlink scan finished in {backlink_scan_elapsed:.1f}s.")
        if has_issues:
            print("\nIssues found.")
            return 1
        print("No missing stows or conflicts found.")
        return 0

    apply_shared_skills(target, verbose=args.verbose)
    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
