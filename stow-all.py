#!/usr/bin/env python3
from __future__ import annotations

import argparse
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
from typing import Final, Literal


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

# Repo slug used in the post-apply Claude plugin instructions.
GITHUB_SLUG: Final[str] = "martintrojer/dotfiles"

# Zsh plugins managed by clone-on-apply. Same model as nvim's vim.pack:
# pinned commit refs in this script, plugins live at ~/.zsh/plugins/<name>/,
# .zshrc sources them directly. Updates: bump the ref, re-run --apply.
# (name, git URL, ref to checkout)
ZSH_PLUGINS: Final[tuple[tuple[str, str, str], ...]] = (
    (
        "zsh-autosuggestions",
        "https://github.com/zsh-users/zsh-autosuggestions",
        "v0.7.1",
    ),
    (
        "zsh-syntax-highlighting",
        "https://github.com/zsh-users/zsh-syntax-highlighting",
        "0.8.0",
    ),
)
# ~/.local/share/zsh-plugins/ rather than ~/.zsh/plugins/ specifically because
# ~/.zsh is a stow-folded symlink into the dotfiles repo source tree; cloning
# under it would dump the plugin contents into the repo. XDG_DATA_HOME-style
# placement keeps third-party clones out of the source tree entirely.
ZSH_PLUGINS_DEST: Final[Path] = Path(".local/share/zsh-plugins")

# TPM (tmux plugin manager). Cloned by --apply so a fresh install doesn't
# need the manual `git clone https://github.com/tmux-plugins/tpm ...` step.
# After apply, first-time bootstrap still needs `prefix + I` inside tmux to
# install the @plugin entries listed in tmux/.tmux.conf -- TPM owns that
# lifecycle, we just bootstrap TPM itself.
#
# Path is fixed at ~/.tmux/plugins/tpm/ because both TPM and tmux.conf
# hardcode it; not configurable.
TPM: Final[tuple[str, str, str]] = (
    "tpm",
    "https://github.com/tmux-plugins/tpm",
    "v3.1.0",
)
TPM_DEST: Final[Path] = Path(".tmux/plugins/tpm")

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
            "glow",
            "jj",
            "local-bin",
            "nvim",
            "opencode",
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

# Top-level directories that exist on disk but are not stow packages and
# should not trip the package-coverage check. Dot-prefixed dirs are auto-
# skipped, so .git/.jj/.claude-plugin/.ruff_cache/.vecgrep don't need to be
# listed here.
IGNORED_TOPLEVEL_DIRS: Final[set[str]] = {
    "__pycache__",
    "agents",       # Claude plugin asset, consumed via `claude plugin install`
    "docs",         # cross-cutting documentation hosted at repo root
    "fedora",       # contains its own stow packages, scope-driven
    "hooks",        # Claude plugin asset, consumed via `claude plugin install`
    "pi",           # source for pi/extensions/, symlinked by --apply
    "skills",       # source for shared skills, symlinked by --apply
    "vscode",       # vscode/podman-host symlink + settings, manual setup
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
    # --no-folding: always create per-leaf symlinks; never fold a
    # directory into a single symlink. Folding was responsible for the
    # "two scopes share a target dir" class of bugs (e.g. local-bin and
    # fedora/bin both contributing to ~/.local/bin/, where the first
    # scope's stow run folded .local/ into a symlink and the second
    # refused to merge). Per-leaf is more verbose but predictable, and
    # `ls -la ~/.config/foo/` always shows where each entry points.
    cmd = ["stow", "--restow", "--no-folding"]
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


def load_repo_stow_ignore_regexes() -> tuple[tuple[str, re.Pattern[str]], ...]:
    """Load repo-managed --ignore regexes from .stowrc.

    .stowrc is the single source of truth for ignore policy. We parse the
    repeated `--ignore=...` lines so `--apply` can also prune legacy links that
    were created before those ignores existed.
    """
    stowrc = SCRIPT_DIR / ".stowrc"
    if not stowrc.is_file():
        return ()

    patterns: list[tuple[str, re.Pattern[str]]] = []
    for line in stowrc.read_text().splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if not stripped.startswith("--ignore="):
            continue
        regex = stripped.removeprefix("--ignore=")
        patterns.append((regex, re.compile(regex)))
    return tuple(patterns)


def repo_path_matches_stow_ignore(
    path: Path,
    specs: dict[str, PackageSpec],
    ignore_patterns: tuple[tuple[str, re.Pattern[str]], ...],
) -> bool:
    owner = owner_for_repo_path(path, specs)
    if owner is None:
        return False

    rel_path = path.relative_to(owner.package_dir.resolve())
    rel_parts = rel_path.parts
    subpaths = [
        "/" + "/".join(rel_parts[start:end])
        for start in range(len(rel_parts))
        for end in range(start + 1, len(rel_parts) + 1)
    ]

    for raw_regex, pattern in ignore_patterns:
        if "/" in raw_regex:
            if any(pattern.fullmatch(candidate) for candidate in subpaths):
                return True
            continue
        if any(pattern.fullmatch(part) for part in rel_parts):
            return True
    return False


def prune_managed_ignored_artifact_links(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    verbose: bool,
) -> None:
    """Remove stale managed links for repo-ignored build/cache artifacts."""
    ignore_patterns = load_repo_stow_ignore_regexes()
    if not ignore_patterns:
        return

    script_real = SCRIPT_DIR.resolve()
    linked_paths: list[Path] = []
    ignored_dirs: set[Path] = set()

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
            if repo_target is None or not repo_path_matches_stow_ignore(
                repo_target, specs, ignore_patterns
            ):
                continue
            linked_paths.append(path)
            parents_to_prune: list[Path] = []
            for target_parent, repo_parent in zip(path.parents, repo_target.parents):
                if target_parent == target:
                    break
                parents_to_prune.append(target_parent)
                if repo_path_matches_stow_ignore(repo_parent, specs, ignore_patterns):
                    ignored_dirs.update(parents_to_prune)
                    break

    if not linked_paths and not ignored_dirs:
        return

    header_printed = False

    def _print_header() -> None:
        nonlocal header_printed
        if not header_printed:
            LOGGER.warning("\n[ignored-artifacts]")
            header_printed = True

    for path in sorted(linked_paths, key=lambda p: len(p.parts), reverse=True):
        try:
            path.unlink()
        except FileNotFoundError:
            continue
        _print_header()
        LOGGER.warning(f"PRUNED: {path.relative_to(target)}")

    for path in sorted(ignored_dirs, key=lambda p: len(p.parts), reverse=True):
        if not path.exists() or not path.is_dir():
            continue
        try:
            path.rmdir()
        except OSError:
            if verbose:
                LOGGER.debug(f"SKIP: {path.relative_to(target)} not empty")
            continue
        _print_header()
        LOGGER.warning(f"REMOVED: {path.relative_to(target)}")


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


# These two helpers are git operations, not zsh-specific. Reused by
# apply_tmux_tpm / check_tmux_tpm below. Kept under the _zsh_plugin
# name to avoid a churn-only rename; promote to _pinned_clone_* if a
# third caller appears.
def _zsh_plugin_head(dest: Path) -> str | None:
    """Return the resolved object SHA at HEAD, or None if not a git repo."""
    result = subprocess.run(
        ["git", "-C", str(dest), "rev-parse", "--verify", "HEAD"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def _zsh_plugin_resolve(dest: Path, ref: str) -> str | None:
    """Resolve the SHA of a ref inside an existing clone, or None if missing."""
    result = subprocess.run(
        ["git", "-C", str(dest), "rev-parse", "--verify", f"{ref}^{{commit}}"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip()


def apply_zsh_plugins(target: Path, *, verbose: bool) -> None:
    """Clone or update zsh plugins listed in ZSH_PLUGINS to the pinned refs.

    Mirrors how nvim/lua/plugins.lua + nvim-pack-lock.json work: the source
    of truth is the version string in this script. .zshrc just sources the
    files at $HOME/.zsh/plugins/<name>/<name>.zsh and trusts they're there.
    """
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
                # Full clone (no --depth) so any pinned tag/SHA in the
                # repo's history is reachable. The repos are small
                # (single-MB range); the depth optimization saved
                # nothing meaningful and silently broke when the pinned
                # ref fell outside the shallow window.
                ["git", "clone", "--quiet", url, str(dest)],
                check=True,
            )
        else:
            # Fetch in case the pinned ref isn't already in the local clone.
            target_sha = _zsh_plugin_resolve(dest, ref)
            if target_sha is None:
                _print_header()
                LOGGER.warning(f"FETCHING: {name} (ref {ref} not in clone)")
                subprocess.run(
                    ["git", "-C", str(dest), "fetch", "--tags", "--quiet"],
                    check=True,
                )
                target_sha = _zsh_plugin_resolve(dest, ref)
            current_sha = _zsh_plugin_head(dest)
            if target_sha is not None and current_sha == target_sha:
                if verbose:
                    LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
                continue
        subprocess.run(
            ["git", "-C", str(dest), "checkout", "--quiet", ref],
            check=True,
        )
        sha = _zsh_plugin_head(dest) or "unknown"
        _print_header()
        LOGGER.warning(f"PINNED: {name} @ {ref} ({sha[:12]})")


def check_zsh_plugins(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    """Verify each ZSH_PLUGINS entry exists at $HOME/.zsh/plugins/<name>/
    and HEAD matches the pinned ref. Used by --check.

    Returns True if any issues were found.
    """
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
        target_sha = _zsh_plugin_resolve(dest, ref)
        current_sha = _zsh_plugin_head(dest)
        if target_sha is None:
            if not found_issue:
                LOGGER.warning("\n[zsh-plugins]")
            LOGGER.warning(
                f"UNKNOWN-REF: {name} ({ref} not in local clone; "
                f"--ignore {issue_id})"
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


def apply_tmux_tpm(target: Path, *, verbose: bool) -> None:
    """Clone or fast-forward TPM to its pinned ref under ~/.tmux/plugins/tpm.

    TPM owns its own @plugin install/update lifecycle (the user runs
    `prefix + I` / `prefix + U` inside tmux). This function only ensures
    TPM itself exists at the right SHA so a fresh-machine bootstrap
    doesn't need a manual git clone before tmux can start.
    """
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
        # See apply_zsh_plugins for why this isn't a shallow clone.
        subprocess.run(
            ["git", "clone", "--quiet", url, str(dest)],
            check=True,
        )
    else:
        target_sha = _zsh_plugin_resolve(dest, ref)
        if target_sha is None:
            _print_header()
            LOGGER.warning(f"FETCHING: {name} (ref {ref} not in clone)")
            subprocess.run(
                ["git", "-C", str(dest), "fetch", "--tags", "--quiet"],
                check=True,
            )
            target_sha = _zsh_plugin_resolve(dest, ref)
        current_sha = _zsh_plugin_head(dest)
        if target_sha is not None and current_sha == target_sha:
            if verbose:
                LOGGER.debug(f"OK: {name} @ {ref} ({target_sha[:12]})")
            return
    subprocess.run(
        ["git", "-C", str(dest), "checkout", "--quiet", ref],
        check=True,
    )
    sha = _zsh_plugin_head(dest) or "unknown"
    _print_header()
    LOGGER.warning(f"PINNED: {name} @ {ref} ({sha[:12]})")


def check_tmux_tpm(target: Path, *, verbose: bool, ignore: set[str]) -> bool:
    """Verify TPM is cloned at the pinned ref. Used by --check."""
    name, _url, ref = TPM
    issue_id = f"tmux-tpm:{name}"
    if issue_id in ignore:
        return False
    dest = target / TPM_DEST
    if not dest.is_dir():
        LOGGER.warning("\n[tmux-tpm]")
        LOGGER.warning(f"MISSING: {name} (--ignore {issue_id})")
        return True
    target_sha = _zsh_plugin_resolve(dest, ref)
    current_sha = _zsh_plugin_head(dest)
    if target_sha is None:
        LOGGER.warning("\n[tmux-tpm]")
        LOGGER.warning(
            f"UNKNOWN-REF: {name} ({ref} not in local clone; "
            f"--ignore {issue_id})"
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
    """Best-effort: Codex's notify hook mentions `agent-attention`.

    Codex is the only agent whose notify wiring isn't owned by `--apply`
    or by the marketplace plugin install. The line in
    `~/.codex/config.toml` is a manual edit (printed as a hint at the
    end of `--apply`), and it's the one most likely to drift on a real
    machine: file grew over time, partial copy from an old machine,
    user forgot the line entirely.

    Other agents are already covered:

    - **Claude** — hook lives in `hooks/hooks.json` and ships via the
      marketplace plugin; any breakage is committed-to-git and visible
      at code-review time.
    - **Pi** — `pi/extensions/agent-attention.ts` is a symlink that
      `--apply` creates and the symlink + stow checks already verify.
    - **OpenCode** — `opencode/.config/opencode/plugin/notify.ts` is
      stowed; same story.

    Deliberately fuzzy: just grep for `agent-attention`. If the user
    has reworked the wiring (different script name, custom command
    shape) the check will warn; that's an acceptable false positive
    for the safety it gives on the common drift cases.
    """
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
            f"NO-NOTIFY: codex config at {path} doesn't mention "
            f"`agent-attention` (--ignore {issue_id})"
        )
        return True
    if verbose:
        LOGGER.debug(f"OK: codex notify hook present in {path}")
    return False


# ---------------------------------------------------------------------------
# Agent skill / pi extension symlinks
#
# Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp etc. all read
# ~/.agents/skills/ as the universal global skills location. Pi reads
# ~/.pi/agent/extensions/ for auto-discovered extensions. Both are simple
# symlink-into-the-repo destinations — no `npx skills`, no `pi install`
# needed. Edits to repo sources are picked up live because the targets
# are symlinks, not copies.
#
# The Claude plugin is the one outlier: Claude wants its own plugin cache
# (it copies the marketplace's content into ~/.claude/plugins/cache/...).
# That stays a manual step — the post-apply hint prints the exact two
# `claude plugin ...` commands. See print_post_apply_hints() below.
# ---------------------------------------------------------------------------


def _agent_link_apply(
    *,
    label: str,
    src_dir: Path,
    dest_dir: Path,
    expected_names: set[str],
    verbose: bool,
) -> None:
    """Symlink each name in expected_names from src_dir into dest_dir, and
    prune any of OUR own stale symlinks (links that point into src_dir but
    no longer have a source).

    Idempotent. Won't touch entries that aren't symlinks (third-party
    skills/extensions in the same dir are left alone). Won't touch
    symlinks that point outside src_dir.
    """
    dest_dir.mkdir(parents=True, exist_ok=True)
    header_printed = False

    def _print_header() -> None:
        nonlocal header_printed
        if not header_printed:
            LOGGER.warning(f"\n[{label}]")
            header_printed = True

    # Create or repair our links.
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

    # Prune our stale links.
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
            # Symlink points outside our source dir — not ours, leave alone.
            continue
        entry.unlink()
        _print_header()
        LOGGER.warning(f"PRUNED: {entry.name} (no source in {src_dir.name}/)")


def apply_skills_symlinks(target: Path, *, verbose: bool) -> None:
    """Symlink each dotfiles/skills/<name>/ into ~/.agents/skills/<name>.

    All supported agents (Codex, OpenCode, Pi, Claude Code, Cursor, Amp,
    Cline, Warp, OpenClaw, ...) read ~/.agents/skills/ as the universal
    global location. We don't need per-agent fan-out (used to use
    `npx skills add`); a plain symlink in the universal path is enough
    and supports live editing.
    """
    src_dir = SCRIPT_DIR / "skills"
    if not src_dir.is_dir():
        return
    expected_names = {
        p.name for p in src_dir.iterdir() if p.is_dir() and not p.name.startswith(".")
    }
    _agent_link_apply(
        label="agents-skills",
        src_dir=src_dir,
        dest_dir=target / ".agents" / "skills",
        expected_names=expected_names,
        verbose=verbose,
    )


def apply_pi_extensions_symlinks(target: Path, *, verbose: bool) -> None:
    """Symlink each dotfiles/pi/extensions/*.ts into ~/.pi/agent/extensions/.

    Pi auto-discovers extensions in ~/.pi/agent/extensions/ (and supports
    /reload for hot-reloading). Plain symlinks work; no `pi install`
    needed.
    """
    src_dir = SCRIPT_DIR / "pi" / "extensions"
    if not src_dir.is_dir():
        return
    expected_names = {
        p.name for p in src_dir.iterdir() if p.is_file() and p.suffix == ".ts"
    }
    _agent_link_apply(
        label="pi-extensions",
        src_dir=src_dir,
        dest_dir=target / ".pi" / "agent" / "extensions",
        expected_names=expected_names,
        verbose=verbose,
    )


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
        has_issues |= check_zsh_plugins(
            target, verbose=args.verbose, ignore=args.ignore
        )
        has_issues |= check_tmux_tpm(
            target, verbose=args.verbose, ignore=args.ignore
        )
        has_issues |= check_agent_notify(
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

    prune_managed_ignored_artifact_links(
        target,
        specs,
        active_names,
        verbose=args.verbose,
    )
    apply_zsh_plugins(target, verbose=args.verbose)
    apply_tmux_tpm(target, verbose=args.verbose)
    apply_skills_symlinks(target, verbose=args.verbose)
    apply_pi_extensions_symlinks(target, verbose=args.verbose)
    print("\nDone.")
    print_post_apply_hints()
    return 0


def print_post_apply_hints() -> None:
    """Print the two manual steps `--apply` cannot do for you.

    Skills (~/.agents/skills/) and pi extensions (~/.pi/agent/extensions/)
    have already been symlinked by --apply itself — those work for
    Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw with no
    further action.

    The two remaining concerns are:
      - Claude Code: needs its own plugin install (Claude copies the
        marketplace content into ~/.claude/plugins/cache/...; there is
        no symlink path in). Re-run after each push to refresh.
      - Codex: needs one TOML line in ~/.codex/config.toml for the
        agent-attention notify hook.
    """
    print()
    print(
        "Skills and pi extensions are now symlinked at "
        "~/.agents/skills/ and ~/.pi/agent/extensions/."
    )
    print(
        "Codex / OpenCode / Pi / Cursor / OpenClaw / etc. read those "
        "paths automatically."
    )
    print()
    print("Two manual steps remain:")
    print()
    print("  # 1. Claude Code plugin (one-time marketplace add, then")
    print("  #    re-run `plugin install` after every push to refresh):")
    print(f"  claude plugin marketplace add {GITHUB_SLUG}")
    print("  claude plugin install mtrojer@dotfiles")
    print()
    print("  # 2. Codex notify hook — add this line to ~/.codex/config.toml:")
    print(
        '  notify = ["/bin/sh", "-lc", "python3 \\"$HOME/.config/tmux/scripts/'
        'agent-attention\\" notify --source codex --event-type notify --title Codex"]'
    )
    print()


if __name__ == "__main__":
    raise SystemExit(main())
