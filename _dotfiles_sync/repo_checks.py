from __future__ import annotations

import logging
import os
import re
from itertools import chain
from pathlib import Path

from .config import SCRIPT_DIR
from .inventory import IGNORED_TOPLEVEL_DIRS
from .model import PackageSpec

LOGGER = logging.getLogger("dotfiles-sync")


def check_package_coverage(specs: dict[str, PackageSpec], *, ignore: set[str]) -> bool:
    expected = set(specs) | IGNORED_TOPLEVEL_DIRS
    found_issue = False
    for child in sorted(SCRIPT_DIR.iterdir(), key=lambda path: path.name):
        if not child.is_dir() or child.name.startswith("."):
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
    """Load repo-managed --ignore regexes from .stowrc."""
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
            for target_parent, repo_parent in zip(
                path.parents, repo_target.parents, strict=False
            ):
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

    for path in sorted(linked_paths, key=lambda item: len(item.parts), reverse=True):
        try:
            path.unlink()
        except FileNotFoundError:
            continue
        _print_header()
        LOGGER.warning(f"PRUNED: {path.relative_to(target)}")

    for path in sorted(ignored_dirs, key=lambda item: len(item.parts), reverse=True):
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
