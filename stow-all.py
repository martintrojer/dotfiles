#!/usr/bin/env python3
from __future__ import annotations

import argparse
import filecmp
import json
import logging
import os
import platform
import re
import shlex
import shutil
import subprocess
import tomllib
import sys
from itertools import chain
from time import monotonic
from dataclasses import dataclass
from datetime import datetime
from pathlib import Path
from typing import Any, Final, Literal, Sequence


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
    ignore: set[str]


SCRIPT_DIR: Final[Path] = Path(__file__).resolve().parent
LOGGER: Final[logging.Logger] = logging.getLogger("stow-all")
SHARED_SKILLS_DIR: Final[Path] = SCRIPT_DIR / "skills"
PI_EXTENSIONS_DIR: Final[Path] = SCRIPT_DIR / "pi" / "extensions"
PI_EXTENSIONS_DEST: Final[Path] = Path(".pi/agent/extensions")
CLAUDE_PLUGIN_DIR: Final[Path] = SCRIPT_DIR / "claude" / "mtrojer-plugin"
CLAUDE_PLUGIN_SKILLS_DIR: Final[Path] = CLAUDE_PLUGIN_DIR / "skills"
CLAUDE_PLUGIN_NAME: Final[str] = "mtrojer"
CLAUDE_PLUGIN_MARKETPLACE: Final[str] = "local"
SHARED_SKILL_LINKS: Final[tuple[tuple[str, Path], ...]] = (
    ("codex", Path(".codex/skills")),
    ("agents", Path(".agents/skills")),
)

AGENT_ATTENTION_SCRIPT: Final[str] = "agent-attention notify"

# Agent notification hooks that can only be checked (manual setup required).
# (label, config_path_relative_to_target, check_function_name)
AGENT_HOOK_CHECKS: Final[tuple[tuple[str, Path, str], ...]] = (
    ("claude", Path(".claude/settings.json"), "claude"),
    ("codex", Path(".codex/config.toml"), "codex"),
)

# Agent notification hook symlinks: (label, source_relative_to_repo, dest_relative_to_target)
# Best-effort: skipped silently if source doesn't exist.
AGENT_HOOK_LINKS: Final[tuple[tuple[str, Path, Path], ...]] = (
    (
        "pi",
        Path("tmux/.config/tmux/scripts/pi-extensions/agent-attention.ts"),
        Path(".pi/agent/extensions/agent-attention.ts"),
    ),
    (
        "opencode",
        Path("tmux/.config/tmux/scripts/opencode-plugin/notify.ts"),
        Path(".config/opencode/plugin/notify.ts"),
    ),
)

# (scope, stow_dir, package_names)
PACKAGE_GROUPS: Final[list[tuple[PackageScope, Path, list[str]]]] = [
    (
        "common",
        SCRIPT_DIR,
        [
            "bat",
            "btop",
            "eza",
            "gdu",
            "git",
            "jj",
            "local-bin",
            "nvim",
            "ssh",
            "summarize",
            "tmux",
            "tuicr",
            "vale",
            "yazi",
            "zsh",
        ],
    ),
    (
        "darwin",
        SCRIPT_DIR,
        ["ghostty", "hammerspoon"],
    ),
    (
        "linux",
        SCRIPT_DIR,
        [
            "alacritty",
            "fuzzel",
            "kanshi",
            "mako",
            "sway",
            "swaylock",
            "waybar",
            "wallpapers",
        ],
    ),
    (
        "fedora",
        SCRIPT_DIR / "fedora",
        [
            "bin",
            "containers",
            "gtk-3.0",
            "systemd",
        ],
    ),
]

IGNORED_TOPLEVEL_DIRS: Final[set[str]] = {
    "__pycache__",
    "claude",
    "fedora",
    "pi",
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
    is_fedora: bool


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
    parser.add_argument(
        "--ignore",
        action="append",
        default=[],
        metavar="ID",
        help="Suppress a specific check failure by ID (repeatable)",
    )
    namespace = parser.parse_args()
    return Args(
        action=namespace.action,
        force_overwrite=namespace.force_overwrite,
        show_diffs=namespace.show_diffs,
        verbose=namespace.verbose,
        target=namespace.target,
        ignore=set(namespace.ignore),
    )


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


def configure_logging(args: Args) -> None:
    logging.basicConfig(
        level=logging.DEBUG if args.verbose else logging.WARNING,
        format="%(message)s",
    )


def build_specs() -> dict[str, PackageSpec]:
    return {
        name: PackageSpec(name=name, stow_dir=stow_dir, scope=scope)
        for scope, stow_dir, names in PACKAGE_GROUPS
        for name in names
    }


def active_scopes(system: SystemInfo) -> set[PackageScope]:
    """Canonical scope selection — all activation rules live here."""
    scopes: set[PackageScope] = {"common"}
    if system.os_name == "Darwin":
        scopes.add("darwin")
    elif system.is_fedora:
        scopes.add("linux")
        scopes.add("fedora")
    return scopes


def ensure_stow_available() -> None:
    if shutil.which("stow") is None:
        LOGGER.error("GNU Stow is required but not installed.")
        raise SystemExit(1)


def meaningful_output(output: str) -> str:
    lines = []
    for line in output.splitlines():
        # Suppress any stow WARNING: line. Currently only the simulation
        # banner exists; matching the prefix keeps us robust to phrasing
        # changes between stow point releases.
        if line.startswith("WARNING:"):
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
    return shlex.quote(str(path))


def check_package_coverage(specs: dict[str, PackageSpec], *, ignore: set[str]) -> bool:
    expected = set(specs) | IGNORED_TOPLEVEL_DIRS
    found_issue = False
    for child in sorted(SCRIPT_DIR.iterdir(), key=lambda p: p.name):
        if not child.is_dir():
            continue
        if child.name.startswith("."):
            continue
        if child.name not in expected:
            issue_id = f"unclassified:{child.name}"
            if issue_id in ignore:
                continue
            if not found_issue:
                LOGGER.warning("\n[package-coverage]")
            found_issue = True
            LOGGER.warning(f"UNCLASSIFIED: {child.name}  (--ignore {issue_id})")
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
        if path.is_relative_to(spec.package_dir.resolve()):
            return spec
    return None


def managed_link_target(path: Path, source_root: Path) -> Path | None:
    if not path.is_symlink():
        return None
    try:
        raw_target = os.readlink(path)
    except OSError:
        return None
    resolved = (path.parent / raw_target).resolve(strict=False)
    if not resolved.is_relative_to(source_root.resolve(strict=False)):
        return None
    return resolved


def extra_managed_backlink_issues(
    dest_dir: Path,
    *,
    source_root: Path,
    expected_names: set[str],
    stale_label: str,
    extra_label: str,
) -> list[str]:
    if not dest_dir.exists() or not dest_dir.is_dir():
        return []

    issues: list[str] = []
    for path in sorted(dest_dir.iterdir()):
        target = managed_link_target(path, source_root)
        if target is None or path.name in expected_names:
            continue
        if target.exists():
            issues.append(f"{path.name}: {extra_label} {target}")
        else:
            issues.append(f"{path.name}: {stale_label} {target}")
    return issues


def check_repo_backlinks(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    ignore: set[str],
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
            repo_target = managed_link_target(path, script_real)
            if repo_target is None:
                continue

            spec = owner_for_repo_path(repo_target, specs)
            if spec is None:
                continue

            rel_path = path.relative_to(target)

            if not path.exists():
                issue_id = f"stale:{rel_path}"
                if issue_id in ignore:
                    continue
                if not found_stale:
                    LOGGER.warning("\n[stale-symlinks]")
                    found_stale = True
                has_issues = True
                LOGGER.warning(f"STALE: {rel_path}  (--ignore {issue_id})")
                continue

            if spec.name not in active_names:
                issue_id = f"invalid:{rel_path}"
                if issue_id in ignore:
                    continue
                if not found_invalid:
                    LOGGER.warning("\n[invalid-backlinks]")
                    found_invalid = True
                has_issues = True
                LOGGER.warning(
                    f"INVALID: {rel_path} [{spec.scope}]  (--ignore {issue_id})"
                )

    return has_issues


def run_check_group(
    label: str,
    stow_dir: Path,
    packages: list[str],
    target: Path,
    show_diffs: bool,
    verbose: bool,
    *,
    ignore: set[str],
) -> bool:
    if not packages:
        return False
    result = run_stow_command(stow_dir, packages, target, simulate=True, verbose=True)
    raw = result.stdout + result.stderr
    output = meaningful_output(raw)
    if not output:
        if verbose:
            LOGGER.debug(f"\n[{label}]")
            LOGGER.debug("OK")
        return False
    conflicts = parse_conflicts(raw)
    ignored_targets = {t for _, t in conflicts if f"conflict:{t}" in ignore}
    remaining = [(s, t) for s, t in conflicts if t not in ignored_targets]
    if not remaining and conflicts:
        return False
    # Re-filter output lines to hide ignored conflicts.
    if ignored_targets:
        output = "\n".join(
            line
            for line in output.splitlines()
            if not any(t in line for t in ignored_targets)
        )
    if not output:
        return False
    LOGGER.warning(f"\n[{label}]")
    sys.stdout.write(f"{output}\n")
    for _, target_rel in remaining:
        LOGGER.warning(f"  (--ignore conflict:{target_rel})")
    if show_diffs:
        show_conflict_diffs(raw, target)
    return True


def run_apply_group(
    label: str,
    stow_dir: Path,
    packages: list[str],
    target: Path,
    verbose: bool,
    force_overwrite: bool,
    backup_root: Path | None,
    *,
    ignore: set[str],
) -> None:
    if not packages:
        return

    # Probe for conflicts, then handle force-overwrite and ignore.
    probe = run_stow_command(stow_dir, packages, target, simulate=True, verbose=True)
    conflicts = parse_conflicts(probe.stdout + probe.stderr)
    if conflicts:
        ignored = {s for s, t in conflicts if f"conflict:{t}" in ignore}
        if ignored:
            # Re-derive which packages to skip: a source like
            # "dotfiles/git/.gitconfig" means the "git" package.
            skip_pkgs = {s.split("/")[1] for s in ignored if "/" in s}
            packages = [p for p in packages if p not in skip_pkgs]
            for pkg in sorted(skip_pkgs):
                LOGGER.warning(f"Skipping package '{pkg}' (ignored conflict)")
            if not packages:
                return

        if force_overwrite:
            LOGGER.warning(f"\n[{label}]")
            filtered = meaningful_output(probe.stdout + probe.stderr)
            if filtered:
                sys.stdout.write(f"{filtered}\n")
            assert backup_root is not None
            for _, target_rel in conflicts:
                if f"conflict:{target_rel}" not in ignore:
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
    """Return only top-level skill directories meant to be linked/copied."""
    return sorted(
        (path for path in SHARED_SKILLS_DIR.iterdir() if path.is_dir()),
        key=lambda path: path.name,
    )


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


def diff_expected_links(
    dest_dir: Path,
    sources: Sequence[Path],
    *,
    extra_issues: list[str] | None = None,
) -> list[str]:
    """Compare the symlinks in ``dest_dir`` against ``sources``.

    Returns a flat ``list[str]`` of human-readable issue lines. ``extra_issues``
    is appended verbatim to allow callers to feed in their own pre-computed
    "unmanaged extras / stale backlinks" detection without re-implementing
    the per-source check.
    """
    issues: list[str] = []
    for source in sources:
        problem = describe_skill_link_state(dest_dir, source)
        if problem is not None:
            issues.append(f"{source.name}: {problem}")
    if extra_issues:
        issues.extend(extra_issues)
    return issues


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


def _shared_skill_link_target(path: Path) -> Path | None:
    return managed_link_target(path, SHARED_SKILLS_DIR)


def _shared_skill_extra_issues(dest_dir: Path, expected_names: set[str]) -> list[str]:
    return extra_managed_backlink_issues(
        dest_dir,
        source_root=SHARED_SKILLS_DIR,
        expected_names=expected_names,
        stale_label="stale backlink to missing",
        extra_label="unmanaged extra link to",
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

    expected_names = {source.name for source in shared_skill_entries()}
    dest_dir.mkdir(parents=True, exist_ok=True)
    for path in sorted(dest_dir.iterdir()):
        target = _shared_skill_link_target(path)
        if target is None or path.name in expected_names:
            continue
        replace_path(path)
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
            source_names = {n for n in source_names if (source_dir / n).is_dir()}
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
    dest.mkdir(parents=True, exist_ok=True)
    remove_dir_contents(dest, keep_names={".gitignore"})
    for source_path in source.iterdir():
        if not source_path.is_dir():
            continue
        shutil.copytree(
            source_path,
            dest / source_path.name,
            copy_function=shutil.copy2,
        )


def print_claude_plugin_hint(*, installed: bool) -> None:
    if installed:
        LOGGER.warning("Refresh the installed Claude Code plugin snapshot:")
    else:
        LOGGER.warning("Install the Claude Code plugin manually:")
        LOGGER.warning(f"  /plugin marketplace add {CLAUDE_PLUGIN_DIR}")
    LOGGER.warning(
        f"  /plugin install {CLAUDE_PLUGIN_NAME}@{CLAUDE_PLUGIN_MARKETPLACE}"
    )


def log_issue_summary(issues: Sequence[str], *, max_items: int = 3) -> None:
    for issue in issues[:max_items]:
        LOGGER.warning(f"  {issue}")
    remaining = len(issues) - max_items
    if remaining > 0:
        LOGGER.warning(f"  ... {remaining} more")


def load_json(path: Path) -> Any:
    if not path.is_file():
        return None
    try:
        return json.loads(path.read_text())
    except (OSError, json.JSONDecodeError):
        return None


def _dig(data: Any, *keys: str) -> Any:
    """Walk nested dicts by key sequence, returning None on any miss."""
    for key in keys:
        if not isinstance(data, dict):
            return None
        data = data.get(key)
    return data


def iter_claude_installed_plugin_roots(target: Path) -> list[Path]:
    roots: set[Path] = set()
    plugins_dir = target / ".claude/plugins"

    entries = _dig(
        load_json(plugins_dir / "installed_plugins.json"),
        "plugins",
        f"{CLAUDE_PLUGIN_NAME}@{CLAUDE_PLUGIN_MARKETPLACE}",
    )
    if isinstance(entries, list):
        for entry in entries:
            install_path = _dig(entry, "installPath")
            if isinstance(install_path, str):
                roots.add(Path(install_path).expanduser())

    return sorted(root.resolve(strict=False) for root in roots)


def iter_claude_marketplace_sources(target: Path) -> list[Path]:
    roots: set[Path] = set()
    plugins_dir = target / ".claude/plugins"

    install_location = _dig(
        load_json(plugins_dir / "known_marketplaces.json"),
        CLAUDE_PLUGIN_MARKETPLACE,
        "installLocation",
    )
    if isinstance(install_location, str):
        roots.add(Path(install_location).expanduser())

    return sorted(root.resolve(strict=False) for root in roots)


def _check_skill_links(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    has_issues = False
    for label, rel_path in SHARED_SKILL_LINKS:
        issue_id = f"skill:{label}"
        if issue_id in ignore:
            continue
        path = target / rel_path
        if path.exists() and not path.is_dir():
            has_issues = True
            detail = f" ({path})" if verbose else ""
            LOGGER.warning(
                f"ISSUE: {label}: expected directory, found file{detail}"
                f"  (--ignore {issue_id})"
            )
            continue
        if not path.exists():
            has_issues = True
            detail = f" ({path})" if verbose else ""
            LOGGER.warning(
                f"ISSUE: {label}: missing directory{detail}  (--ignore {issue_id})"
            )
            continue

        conflicts = check_shared_skill_link_conflicts(path)
        if conflicts:
            has_issues = True
            LOGGER.warning(
                f"ISSUE: {label}: {len(conflicts)} unmanaged skill conflicts"
                f"  (--ignore {issue_id})"
            )
            if verbose:
                LOGGER.debug(f"  {path}")
                log_issue_summary(conflicts, max_items=10)
            else:
                log_issue_summary(conflicts)
            continue

        sources = shared_skill_entries()
        expected_names = {source.name for source in sources}
        issues = diff_expected_links(
            path,
            sources,
            extra_issues=_shared_skill_extra_issues(path, expected_names),
        )

        if issues:
            has_issues = True
            LOGGER.warning(
                f"ISSUE: {label}: {len(issues)} skill links out of sync"
                f"  (--ignore {issue_id})"
            )
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
    return has_issues


def _check_claude_bundle(*, verbose: bool) -> bool:
    bundle_issues = compare_skill_trees(SHARED_SKILLS_DIR, CLAUDE_PLUGIN_SKILLS_DIR)
    if bundle_issues:
        LOGGER.warning(
            f"ISSUE: claude bundle: {len(bundle_issues)} differences"
            "  (--ignore skill:claude-bundle)"
        )
        if verbose:
            LOGGER.debug(f"  {CLAUDE_PLUGIN_SKILLS_DIR}")
            log_issue_summary(bundle_issues, max_items=10)
        else:
            log_issue_summary(bundle_issues)
        return True
    if verbose:
        LOGGER.debug(f"OK: claude bundle ({CLAUDE_PLUGIN_SKILLS_DIR})")
    else:
        LOGGER.info("OK: claude bundle")
    return False


def _check_claude_plugin(target: Path, *, verbose: bool) -> bool:
    marketplace_sources = iter_claude_marketplace_sources(target)
    if marketplace_sources and verbose:
        LOGGER.debug("\n[claude-marketplace]")
        for root in marketplace_sources:
            LOGGER.debug(f"OK: source ({root})")

    plugin_roots = iter_claude_installed_plugin_roots(target)
    if not plugin_roots:
        LOGGER.warning("ISSUE: claude plugin not installed  (--ignore skill:plugin)")
        print_claude_plugin_hint(installed=False)
        return True

    LOGGER.info("[claude-plugin]")
    has_issues = False
    for root in plugin_roots:
        plugin_skills_dir = root / "skills"
        issues = compare_skill_trees(SHARED_SKILLS_DIR, plugin_skills_dir)
        if issues:
            has_issues = True
            LOGGER.warning(
                f"ISSUE: installed copy stale: {len(issues)} differences"
                "  (--ignore skill:plugin)"
            )
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


def _pi_extension_entries() -> list[Path]:
    """Return all .ts files in the pi/extensions directory."""
    if not PI_EXTENSIONS_DIR.is_dir():
        return []
    return sorted(p for p in PI_EXTENSIONS_DIR.iterdir() if p.suffix == ".ts")


def _pi_extension_link_target(path: Path) -> Path | None:
    return managed_link_target(path, PI_EXTENSIONS_DIR)


def _pi_extension_extra_issues(dest_dir: Path, expected_names: set[str]) -> list[str]:
    return extra_managed_backlink_issues(
        dest_dir,
        source_root=PI_EXTENSIONS_DIR,
        expected_names=expected_names,
        stale_label="stale backlink to missing",
        extra_label="unmanaged extra link to",
    )


def _check_pi_extensions(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    issue_id = "pi:extensions"
    if issue_id in ignore:
        return False
    dest_dir = target / PI_EXTENSIONS_DEST
    entries = _pi_extension_entries()
    expected_names = {source.name for source in entries}
    if not entries and not dest_dir.exists():
        return False
    issues = diff_expected_links(
        dest_dir,
        entries,
        extra_issues=_pi_extension_extra_issues(dest_dir, expected_names),
    )
    if issues:
        LOGGER.warning(
            f"ISSUE: pi extensions: {len(issues)} links out of sync"
            f"  (--ignore {issue_id})"
        )
        if verbose:
            LOGGER.debug(f"  {dest_dir}")
        log_issue_summary(issues, max_items=10)
        return True
    if verbose:
        LOGGER.debug(f"OK: pi extensions ({dest_dir})")
    return False


def _apply_pi_extensions(target: Path, *, verbose: bool, ignore: set[str]) -> None:
    issue_id = "pi:extensions"
    if issue_id in ignore:
        return
    dest_dir = target / PI_EXTENSIONS_DEST
    entries = _pi_extension_entries()
    if not entries and not dest_dir.exists():
        return
    dest_dir.mkdir(parents=True, exist_ok=True)
    expected_names = {source.name for source in entries}
    for path in sorted(dest_dir.iterdir()):
        target_path = _pi_extension_link_target(path)
        if target_path is None or path.name in expected_names:
            continue
        replace_path(path)
        if verbose:
            LOGGER.debug(f"REMOVED: stale pi ext {path.name}")
        else:
            LOGGER.info(f"REMOVED: pi ext {path.name}")
    for source in entries:
        link_path = dest_dir / source.name
        desired = source.resolve(strict=False)
        if link_path.is_symlink() and link_path.resolve(strict=False) == desired:
            if verbose:
                LOGGER.debug(f"OK: {source.name}")
            continue
        if link_path.exists() or link_path.is_symlink():
            replace_path(link_path)
        link_path.symlink_to(os.path.relpath(source, start=link_path.parent))
        if verbose:
            LOGGER.debug(f"LINKED: {source.name} -> {link_path}")
        else:
            LOGGER.info(f"LINKED: pi ext {source.name}")


def check_shared_skills(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    has_issues = False
    LOGGER.info("\n[shared-skills]")
    has_issues |= _check_skill_links(target, verbose=verbose, ignore=ignore)
    if "skill:claude-bundle" not in ignore:
        has_issues |= _check_claude_bundle(verbose=verbose)
    if "skill:plugin" not in ignore:
        has_issues |= _check_claude_plugin(target, verbose=verbose)
    LOGGER.info("\n[pi-extensions]")
    has_issues |= _check_pi_extensions(target, verbose=verbose, ignore=ignore)
    return has_issues


def apply_shared_skills(target: Path, *, verbose: bool, ignore: set[str]) -> None:
    LOGGER.info("\n[shared-skills]")
    for label, rel_path in SHARED_SKILL_LINKS:
        if f"skill:{label}" in ignore:
            continue
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

    if "skill:claude-bundle" not in ignore:
        sync_skill_tree(SHARED_SKILLS_DIR, CLAUDE_PLUGIN_SKILLS_DIR)
        if verbose:
            LOGGER.debug(f"SYNCED: claude-plugin bundle -> {CLAUDE_PLUGIN_SKILLS_DIR}")
        else:
            LOGGER.info("SYNCED: claude-plugin bundle")

    if "skill:plugin" in ignore:
        return

    marketplace_sources = iter_claude_marketplace_sources(target)
    if verbose and marketplace_sources:
        LOGGER.debug("\n[claude-marketplace]")
        for root in marketplace_sources:
            LOGGER.debug(f"OK: source ({root})")

    plugin_roots = iter_claude_installed_plugin_roots(target)
    if not plugin_roots:
        LOGGER.warning("Claude Code plugin install still requires one manual step.")
        print_claude_plugin_hint(installed=False)
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
        print_claude_plugin_hint(installed=True)


def _check_claude_hook(config_path: Path) -> str | None:
    """Check if Claude Code settings.json has the agent-attention notification hook."""
    data = load_json(config_path)
    if data is None:
        return "config not found"
    hooks = _dig(data, "hooks", "Notification")
    if not isinstance(hooks, list) or not hooks:
        return "no Notification hook configured"
    for entry in hooks:
        if not isinstance(entry, dict):
            continue
        entry_hooks = entry.get("hooks", [])
        if not isinstance(entry_hooks, list):
            continue
        for hook in entry_hooks:
            if not isinstance(hook, dict):
                continue
            cmd = hook.get("command", "")
            if not isinstance(cmd, str):
                continue
            if AGENT_ATTENTION_SCRIPT in cmd and "--source claude" in cmd:
                return None
    return "agent-attention hook not found in Notification hooks"


def _check_codex_hook(config_path: Path) -> str | None:
    """Check if Codex config.toml has the agent-attention notify command.

    Codex's ``notify`` key is a TOML array of strings invoked as argv
    (typically ``["/bin/sh", "-lc", "... agent-attention notify --source
    codex ..."]``). Parse the file with ``tomllib`` and inspect that key
    directly so comments and unrelated keys can't false-positive.
    """
    if not config_path.is_file():
        return "config not found"
    try:
        with config_path.open("rb") as fh:
            data = tomllib.load(fh)
    except OSError:
        return "cannot read config"
    except tomllib.TOMLDecodeError as exc:
        return f"invalid TOML: {exc}"

    notify = data.get("notify")
    if not isinstance(notify, list) or not notify:
        return "no notify command configured"
    joined = " ".join(part for part in notify if isinstance(part, str))
    # The script path can be quoted (codex's TOML array form requires it for
    # ``$HOME`` expansion via ``sh -lc``), so just check for the script's
    # filename plus the codex-specific ``--source`` flag.
    if "agent-attention" in joined and "--source codex" in joined:
        return None
    return "agent-attention hook not found in notify command"


def check_agent_hooks(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    has_issues = False

    # Check-only hooks (manual setup required)
    check_fns = {"claude": _check_claude_hook, "codex": _check_codex_hook}
    for label, config_rel, fn_key in AGENT_HOOK_CHECKS:
        issue_id = f"hook:{label}"
        if issue_id in ignore:
            continue
        config_path = target / config_rel
        problem = check_fns[fn_key](config_path)
        if problem is None:
            if verbose:
                LOGGER.debug(f"OK: {label} hook ({config_path})")
        else:
            has_issues = True
            LOGGER.warning(
                f"ISSUE: {label} hook: {problem} (manual setup, --ignore {issue_id})"
            )

    # Symlink-based hooks
    for label, source_rel, dest_rel in AGENT_HOOK_LINKS:
        issue_id = f"hook:{label}"
        if issue_id in ignore:
            continue
        source = SCRIPT_DIR / source_rel
        if not source.is_file():
            continue  # best-effort: skip if source doesn't exist
        dest = target / dest_rel
        if dest.is_symlink() and dest.resolve(strict=False) == source.resolve(
            strict=False
        ):
            if verbose:
                LOGGER.debug(f"OK: {label} hook ({dest})")
            continue
        if not dest.exists() and not dest.is_symlink():
            has_issues = True
            LOGGER.warning(f"ISSUE: {label} hook: missing  (--ignore {issue_id})")
            continue
        has_issues = True
        LOGGER.warning(f"ISSUE: {label} hook: wrong target  (--ignore {issue_id})")
    return has_issues


def apply_agent_hooks(target: Path, *, verbose: bool, ignore: set[str]) -> None:
    for label, source_rel, dest_rel in AGENT_HOOK_LINKS:
        if f"hook:{label}" in ignore:
            continue
        source = SCRIPT_DIR / source_rel
        if not source.is_file():
            continue  # best-effort: skip if source doesn't exist
        dest = target / dest_rel
        if dest.is_symlink() and dest.resolve(strict=False) == source.resolve(
            strict=False
        ):
            if verbose:
                LOGGER.debug(f"OK: {label} hook ({dest})")
            continue
        dest.parent.mkdir(parents=True, exist_ok=True)
        if dest.exists() or dest.is_symlink():
            replace_path(dest)
        dest.symlink_to(os.path.relpath(source, start=dest.parent))
        if verbose:
            LOGGER.debug(f"LINKED: {label} hook -> {dest}")
        else:
            LOGGER.info(f"LINKED: {label} hook")


def main() -> int:
    args = parse_args()
    ensure_stow_available()

    target = Path(args.target).expanduser()
    if not target.is_dir():
        LOGGER.error(f"Target directory does not exist: {target}")
        return 1

    configure_logging(args)

    system = detect_system()
    specs = build_specs()
    scopes = active_scopes(system)
    active_names = {name for name, spec in specs.items() if spec.scope in scopes}
    backup_root = None
    if args.force_overwrite:
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        backup_root = target / ".stow-all-backups" / timestamp

    LOGGER.info(f"Using target: {target}")
    LOGGER.info(f"Detected OS: {system.os_name}")
    LOGGER.info(f"Mode: {args.action}")
    if backup_root is not None:
        LOGGER.info(
            "Force overwrite enabled; conflicting targets will be moved to: "
            f"{backup_root}"
        )

    # Group active packages by stow_dir for batched stow calls.
    groups: dict[tuple[str, Path], list[str]] = {}
    for name in sorted(active_names):
        spec = specs[name]
        key = (spec.scope, spec.stow_dir)
        groups.setdefault(key, []).append(name)

    has_issues = False
    for (label, stow_dir), packages in groups.items():
        if args.action == "check":
            has_issues |= run_check_group(
                label,
                stow_dir,
                packages,
                target,
                args.show_diffs,
                args.verbose,
                ignore=args.ignore,
            )
        else:
            run_apply_group(
                label,
                stow_dir,
                packages,
                target,
                verbose=args.verbose,
                force_overwrite=args.force_overwrite,
                backup_root=backup_root,
                ignore=args.ignore,
            )

    if args.action == "check":
        has_issues |= check_package_coverage(specs, ignore=args.ignore)
        has_issues |= check_shared_skills(
            target, verbose=args.verbose, ignore=args.ignore
        )
        LOGGER.info("\n[agent-hooks]")
        has_issues |= check_agent_hooks(
            target, verbose=args.verbose, ignore=args.ignore
        )
        LOGGER.info("\n[repo-backlinks]")
        LOGGER.info("Scanning for stale or invalid repo backlinks...")
        backlink_scan_started = monotonic()
        has_issues |= check_repo_backlinks(
            target, specs, active_names, ignore=args.ignore
        )
        backlink_scan_elapsed = monotonic() - backlink_scan_started
        LOGGER.info(f"Backlink scan finished in {backlink_scan_elapsed:.1f}s.")
        if has_issues:
            print("\nIssues found.")
            return 1
        print("No missing stows or conflicts found.")
        return 0

    apply_shared_skills(target, verbose=args.verbose, ignore=args.ignore)
    LOGGER.info("\n[pi-extensions]")
    _apply_pi_extensions(target, verbose=args.verbose, ignore=args.ignore)
    LOGGER.info("\n[agent-hooks]")
    apply_agent_hooks(target, verbose=args.verbose, ignore=args.ignore)
    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
