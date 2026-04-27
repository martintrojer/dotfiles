from __future__ import annotations

import filecmp
import logging
import shlex
import shutil
import subprocess
import sys
from collections.abc import Sequence
from pathlib import Path

from .config import CONFLICT_RE, SCRIPT_DIR
from .model import Conflict

LOGGER = logging.getLogger("dotfiles-sync")


def ensure_stow_available() -> None:
    if shutil.which("stow") is None:
        LOGGER.error("GNU Stow is required but not installed.")
        raise SystemExit(1)


def meaningful_output(output: str) -> str:
    lines: list[str] = []
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
    if ignored_targets:
        output = "\n".join(
            line
            for line in output.splitlines()
            if not any(target_rel in line for target_rel in ignored_targets)
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

    probe = run_stow_command(stow_dir, packages, target, simulate=True, verbose=True)
    conflicts = parse_conflicts(probe.stdout + probe.stderr)
    if conflicts:
        ignored = {
            source
            for source, target_rel in conflicts
            if f"conflict:{target_rel}" in ignore
        }
        if ignored:
            skip_pkgs = {source.split("/")[1] for source in ignored if "/" in source}
            packages = [package for package in packages if package not in skip_pkgs]
            for package in sorted(skip_pkgs):
                LOGGER.warning(f"Skipping package '{package}' (ignored conflict)")
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
