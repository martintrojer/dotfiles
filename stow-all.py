#!/usr/bin/env python3
from __future__ import annotations

import argparse
import filecmp
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
from typing import Final, Literal, Sequence


Action = Literal["check", "apply"]
PackageScope = Literal["common", "darwin", "linux", "fedora"]
Conflict = tuple[str, str]


@dataclass(frozen=True)
class Args:
    action: Action
    force_overwrite: bool
    show_diffs: bool
    quiet: bool
    target: str


SCRIPT_PATH = Path(__file__).resolve()
SCRIPT_DIR: Final[Path] = SCRIPT_PATH.parent

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
    "codex",
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
        "-q",
        "--quiet",
        action="store_true",
        help="Reduce stow verbosity",
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
        quiet=namespace.quiet,
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
        print("GNU Stow is required but not installed.", file=sys.stderr)
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
            print("Diffs:")
            found = True
        if filecmp.cmp(target_path, source_path, shallow=False):
            print(
                f"  {target_rel} matches repo copy; "
                "safe to replace with --apply --force-overwrite"
            )
        else:
            print(f"  diff -u {shlex_quote(target_path)} {shlex_quote(source_path)}")


def backup_conflict_path(rel_path: str, target: Path, backup_root: Path) -> None:
    target_path = target / rel_path
    if not target_path.exists() and not target_path.is_symlink():
        return
    backup_path = backup_root / rel_path
    backup_path.parent.mkdir(parents=True, exist_ok=True)
    shutil.move(str(target_path), str(backup_path))
    print(f"BACKED UP: {target_path} -> {backup_path}")


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
                print("\n[package-coverage]")
            found_issue = True
            print(f"UNCLASSIFIED: {child.name}")
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
                    print("\n[stale-symlinks]")
                    found_stale = True
                has_issues = True
                print(f"STALE: {rel_path} -> {repo_target}")
                continue

            if spec.name not in active_names:
                if not found_invalid:
                    print("\n[invalid-backlinks]")
                    found_invalid = True
                has_issues = True
                print(f"INVALID: {rel_path} -> {repo_target} [{spec.scope}]")

    return has_issues


def run_check_group(
    label: str,
    stow_dir: Path,
    packages: list[str],
    target: Path,
    show_diffs: bool,
) -> bool:
    if not packages:
        return False
    result = run_stow_command(stow_dir, packages, target, simulate=True, verbose=True)
    output = meaningful_output(result.stdout + result.stderr)
    if not output:
        return False
    print(f"\n[{label}]")
    print(output)
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
            print(f"\n[{label}]")
            filtered = meaningful_output(probe.stdout + probe.stderr)
            if filtered:
                print(filtered)
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


def main() -> int:
    args = parse_args()
    ensure_stow_available()

    target = Path(args.target).expanduser()
    if not target.is_dir():
        print(f"Target directory does not exist: {target}", file=sys.stderr)
        return 1

    system = detect_system()
    specs = build_specs()
    active_names_list, _inactive_names_list = active_package_names(system)
    active_names = set(active_names_list)
    backup_root = None
    if args.force_overwrite:
        timestamp = datetime.now().strftime("%Y%m%d-%H%M%S")
        backup_root = target / ".stow-all-backups" / timestamp

    print(f"Using target: {target}")
    print(f"Detected OS: {system.os_name}")
    if system.distro_id:
        if system.distro_like:
            print(f"Detected distro: {system.distro_id} (like: {system.distro_like})")
        else:
            print(f"Detected distro: {system.distro_id}")
    print(f"Mode: {args.action}")
    if backup_root is not None:
        print(
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
            verbose=not args.quiet,
            force_overwrite=args.force_overwrite,
            backup_root=backup_root,
        )

    if system.os_name == "Darwin":
        if args.action == "check":
            has_issues |= run_check_group(
                "darwin", SCRIPT_DIR, DARWIN_PACKAGES, target, args.show_diffs
            )
        else:
            run_apply_group(
                "darwin",
                SCRIPT_DIR,
                DARWIN_PACKAGES,
                target,
                verbose=not args.quiet,
                force_overwrite=args.force_overwrite,
                backup_root=backup_root,
            )
    elif system.os_name == "Linux":
        if args.action == "check":
            has_issues |= run_check_group(
                "linux", SCRIPT_DIR, LINUX_PACKAGES, target, args.show_diffs
            )
        else:
            run_apply_group(
                "linux",
                SCRIPT_DIR,
                LINUX_PACKAGES,
                target,
                verbose=not args.quiet,
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
                )
            else:
                run_apply_group(
                    "fedora",
                    SCRIPT_DIR / "fedora",
                    FEDORA_PACKAGES,
                    target,
                    verbose=not args.quiet,
                    force_overwrite=args.force_overwrite,
                    backup_root=backup_root,
                )
        elif system.is_debian_family:
            print("Skipping fedora/* packages on Debian-family distro.")
        else:
            print("Skipping fedora/* packages on non-Fedora Linux distro.")
    else:
        print(f"Skipping OS-specific packages for unsupported OS: {system.os_name}")

    has_issues |= check_package_coverage(specs)

    if args.action == "check":
        print("\n[repo-backlinks]")
        print("Scanning for stale or invalid repo backlinks...")
        backlink_scan_started = monotonic()
        has_issues |= check_repo_backlinks(target, specs, active_names)
        backlink_scan_elapsed = monotonic() - backlink_scan_started
        print(f"Backlink scan finished in {backlink_scan_elapsed:.1f}s.")
        if has_issues:
            print("\nIssues found.")
            return 1
        print("No missing stows or conflicts found.")
        return 0

    print("Done.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
