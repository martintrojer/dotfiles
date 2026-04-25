#!/usr/bin/env python3
from __future__ import annotations

import argparse
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
    install_agents: bool


SCRIPT_DIR: Final[Path] = Path(__file__).resolve().parent
LOGGER: Final[logging.Logger] = logging.getLogger("stow-all")

# Repo URL used by the install commands printed at the end of --apply.
# Local-path equivalents work too; see the install hints in main().
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
    "agents",       # Claude plugin asset, consumed via /plugin install
    "commands",     # Claude plugin asset
    "fedora",       # contains its own stow packages, scope-driven
    "hooks",        # Claude plugin asset
    "pi",           # source for pi/extensions, consumed via pi install
    "skills",       # source for shared skills, consumed via npx skills add
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
    parser.add_argument(
        "--install-agents",
        action="store_true",
        help=(
            "After --apply, install/refresh the Claude Code plugin from the "
            f"github marketplace ({GITHUB_SLUG}). Idempotent: re-runs only "
            "when the repo's git HEAD has advanced past the cached snapshot. "
            "Skills and pi extensions are always symlinked into ~/.agents/skills/ "
            "and ~/.pi/agent/extensions/ by --apply itself \u2014 no flag needed. "
            "Codex's notify hook is the only remaining manual step (printed)."
        ),
    )
    namespace = parser.parse_args()
    return Args(
        action=namespace.action,
        force_overwrite=namespace.force_overwrite,
        show_diffs=namespace.show_diffs,
        verbose=namespace.verbose,
        target=namespace.target,
        ignore=set(namespace.ignore),
        install_agents=namespace.install_agents,
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
                ["git", "clone", "--quiet", "--depth", "50", url, str(dest)],
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
# That's handled by install_claude_plugin() behind the --install-agents
# flag because it's slow and network-y.
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

    apply_zsh_plugins(target, verbose=args.verbose)
    apply_skills_symlinks(target, verbose=args.verbose)
    apply_pi_extensions_symlinks(target, verbose=args.verbose)
    print("\nDone.")
    if args.install_agents:
        install_claude_plugin()
        print_codex_hint()
    else:
        print_publish_reminder()
    return 0


def print_publish_reminder() -> None:
    """After --apply (without --install-agents), tell the user what's left
    to do for the Claude plugin and the Codex notify hook.

    Skills (~/.agents/skills/) and pi extensions (~/.pi/agent/extensions/)
    have already been symlinked by --apply itself — those work for
    Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw with no
    further action.

    The two remaining concerns are:
      - Claude Code: needs its own plugin install (handled by
        --install-agents, or by the manual commands below).
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
    print("To finish the setup (or re-run with --install-agents):")
    print()
    print("  # Claude Code plugin (refresh after each push):")
    print(f"  claude plugin marketplace add {GITHUB_SLUG}")
    print("  claude plugin install mtrojer@dotfiles")
    print()
    print("  # Codex notify hook (manual; add to ~/.codex/config.toml):")
    print(
        '  notify = ["/bin/sh", "-lc", "python3 \\"$HOME/.config/tmux/scripts/'
        'agent-attention\\" notify --source codex --event-type notify --title Codex"]'
    )
    print()


def _run_install(label: str, cmd: list[str]) -> bool:
    """Run an install command, log it, and return True on success.

    We want output streamed live (these commands can be slow + interactive),
    so no capture_output. Errors are logged but not fatal — a missing CLI
    on one agent shouldn't block the other installs.
    """
    LOGGER.warning(f"\n[install: {label}]")
    LOGGER.warning(f"  $ {' '.join(cmd)}")
    try:
        result = subprocess.run(cmd)
    except FileNotFoundError:
        LOGGER.warning(f"  SKIPPED: {cmd[0]} not on PATH")
        return False
    if result.returncode != 0:
        LOGGER.warning(f"  FAILED: {label} (exit {result.returncode})")
        return False
    return True


def _current_repo_head() -> str | None:
    """Return the full SHA of the repo's git HEAD, or None on failure.

    Used to compare against the SHA Claude has cached for our plugin so
    we only re-install when the repo has actually advanced. In jj+git
    colocation, `git rev-parse HEAD` returns the last jj-committed
    change — which is what `claude plugin install` (using the github
    remote) will resolve to once we push.
    """
    result = subprocess.run(
        ["git", "-C", str(SCRIPT_DIR), "rev-parse", "HEAD"],
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return None
    return result.stdout.strip() or None


def _claude_installed_sha() -> str | None:
    """Return the gitCommitSha Claude has for mtrojer@dotfiles (user scope),
    or None if not installed / state unreadable.
    """
    state = Path.home() / ".claude" / "plugins" / "installed_plugins.json"
    if not state.is_file():
        return None
    try:
        data = json.loads(state.read_text())
    except (OSError, ValueError):
        return None
    entries = data.get("plugins", {}).get("mtrojer@dotfiles")
    if not entries:
        return None
    for entry in entries:
        if entry.get("scope") == "user":
            return entry.get("gitCommitSha")
    return None


def install_claude_plugin() -> None:
    """Idempotent install/refresh of the dotfiles Claude plugin.

    Uses the github marketplace (`martintrojer/dotfiles`) rather than a
    local-path install, so the canonical update loop is
    'edit → commit → push → stow-all.py --apply --install-agents'. Local
    iteration on the plugin itself can be done by running
    `claude plugin marketplace add /path/to/dotfiles` manually.

    Skips if `claude` isn't on PATH. Marketplace add is one-time. Plugin
    install is re-run only when the repo's git HEAD has advanced past
    the cached snapshot SHA — so edits to skills/agents/commands/hooks
    propagate after a push.
    """
    if shutil.which("claude") is None:
        LOGGER.warning("\n[install: claude] SKIPPED: claude CLI not on PATH")
        return

    list_out = subprocess.run(
        ["claude", "plugin", "marketplace", "list"],
        capture_output=True,
        text=True,
    )
    if "dotfiles" not in list_out.stdout:
        ok = _run_install(
            "claude marketplace",
            ["claude", "plugin", "marketplace", "add", GITHUB_SLUG],
        )
        if not ok:
            LOGGER.warning(
                "[install: claude plugin] SKIPPED: marketplace add failed; "
                "check that the github remote has the .claude-plugin/ "
                "directory committed and pushed."
            )
            return
    else:
        LOGGER.warning(
            "\n[install: claude marketplace] already registered (dotfiles)"
        )

    repo_head = _current_repo_head()
    installed_sha = _claude_installed_sha()

    if repo_head is None:
        LOGGER.warning(
            "[install: claude plugin] WARNING: could not read repo HEAD; "
            "installing unconditionally"
        )
        needs_install = True
    elif installed_sha is None:
        needs_install = True
    elif installed_sha == repo_head:
        LOGGER.warning(
            f"[install: claude plugin] already installed at "
            f"{installed_sha[:12]} (matches repo HEAD)"
        )
        needs_install = False
    else:
        LOGGER.warning(
            f"[install: claude plugin] cached={installed_sha[:12]} "
            f"repo HEAD={repo_head[:12]} \u2014 reinstalling"
        )
        needs_install = True

    if needs_install:
        _run_install(
            "claude plugin",
            ["claude", "plugin", "install", "mtrojer@dotfiles"],
        )


def print_codex_hint() -> None:
    """Codex's notify hook is the one thing that stays a manual edit.

    Codex has no plugin marketplace, and editing ~/.codex/config.toml
    in-place would require parsing/rewriting the user's TOML — not worth
    the schema-rewriting risk for one line.
    """
    LOGGER.warning(
        "\n[install: codex] still manual. Add this single line to "
        "~/.codex/config.toml:\n\n"
        '    notify = ["/bin/sh", "-lc", "python3 \\"$HOME/.config/tmux/'
        'scripts/agent-attention\\" notify --source codex --event-type '
        'notify --title Codex"]'
    )


if __name__ == "__main__":
    raise SystemExit(main())
