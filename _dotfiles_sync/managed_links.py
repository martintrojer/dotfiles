from __future__ import annotations

import logging
import os
from collections.abc import Iterable, Iterator
from itertools import chain
from pathlib import Path
from typing import Final

from .config import REPO_ROOT, lazy_header
from .ignore import IgnoreRules
from .model import PackageSpec

LOGGER = logging.getLogger("dotfiles-sync")

# How far below a mirrored directory the orphan scan will descend. The repo's
# own deleted shapes are shallow (a systemd `*.service.d/` drop-in is one
# level), and this is the only thing making the scan's cost independent of
# what happens to sit under a mirrored path -- see iter_orphaned_managed_links.
MAX_ORPHAN_DEPTH: Final[int] = 3


def collect_scan_roots(
    specs: dict[str, PackageSpec], target: Path, active_names: set[str]
) -> list[Path]:
    """Every directory under `target` that some package mirrors, plus `target`.

    Only these can hold a managed link: the planner never creates one outside a
    path shape the repo mirrors. Scanning exactly this set replaces a recursive
    walk of each top-level root, which on Linux meant descending all of
    `~/.local` -- 450k entries of Steam depots, mise installs, and container
    layers -- to find symlinks that only ever live in 76 known directories.
    macOS never felt it because those trees aren't there.

    Directories are returned parent-first so the prune step's parent walk still
    sees a containing directory before what it contains.
    """
    roots: set[Path] = {target}
    for name in active_names:
        spec = specs[name]
        if not spec.package_dir.is_dir():
            continue
        for child in spec.package_dir.rglob("*"):
            if not child.is_dir() or child.is_symlink():
                continue
            rel = child.relative_to(spec.package_dir)
            # A bundle's *children* link whole, so their contents live in the
            # repo, not under `target`; recursing would invent target paths
            # that cannot exist. The bundle dir itself is still a scan root,
            # because that is exactly where those child links land -- without
            # it, deleting a bundled item (a skill) stranded its symlink in
            # $HOME with no scan root able to see it, so neither --check nor
            # the prune step could ever report or clear it.
            if any(rel.is_relative_to(bundle) for bundle in spec.bundle_dirs):
                if rel in spec.bundle_dirs:
                    roots.add(target / rel)
                continue
            roots.add(target / rel)
    return sorted(roots, key=lambda path: len(path.parts))


def owner_for_repo_path(
    path: Path, specs: dict[str, PackageSpec]
) -> PackageSpec | None:
    for spec in specs.values():
        if path.is_relative_to(spec.package_dir.resolve()):
            return spec
    return None


def repo_path_is_ignored(
    path: Path,
    specs: dict[str, PackageSpec],
    rules: IgnoreRules,
) -> bool:
    """True if the linker would skip this repo path.

    Shares one rule set with the planner (ignore.py). This used to be a
    second, hand-maintained implementation of stow's matching semantics so the
    backlink audit could agree with what stow had actually linked.
    """
    owner = owner_for_repo_path(path, specs)
    if owner is None:
        return False
    return rules.matches(path.relative_to(owner.package_dir.resolve()))


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


def iter_orphaned_managed_links(
    scan_root: Path, mirrored: set[Path], script_real: Path, depth: int = 1
) -> Iterator[tuple[Path, Path]]:
    """Yield dangling managed links in subdirs the repo no longer mirrors.

    collect_scan_roots only knows what the repo mirrors *now*, and each root is
    read one level deep, so deleting a whole mirrored directory hid everything
    inside it: no repo dir was left to derive a scan root from, and its links
    sat a level too deep for the parent's scan. `--check` then called the tree
    clean while `--apply` had nothing to prune, which is how a removed Sunshine
    unit drop-in stayed symlinked into `$HOME` across runs.

    Only *dangling* links are yielded, which is exactly the formerly-mirrored
    case: a path the repo no longer mirrors cannot have a live link. A link
    that still resolves in such a directory was not put there by the planner,
    so it stays somebody else's business (the stray-link case).

    Two bounds keep this off the 450k-entry trees that made collect_scan_roots
    stop walking recursively in the first place. Descent stops at any directory
    holding a real file, because a leftover of a deleted mirrored directory
    holds only our links and more such directories. That signal alone is too
    weak to rely on -- `~/.local/share` is spared by two stray files at its top
    and `~/.var/app` has none -- so MAX_ORPHAN_DEPTH is the actual guarantee.
    """
    if depth > MAX_ORPHAN_DEPTH:
        return
    try:
        entries = sorted(scan_root.iterdir())
    except OSError:
        return

    if any(not item.is_symlink() and not item.is_dir() for item in entries):
        return

    for item in entries:
        if item.is_symlink():
            if item.exists():
                continue
            repo_target = managed_link_target(item, script_real)
            if repo_target is not None:
                yield item, repo_target
        elif item.is_dir() and item not in mirrored:
            yield from iter_orphaned_managed_links(
                item, mirrored, script_real, depth + 1
            )


def iter_managed_links(
    target: Path, specs: dict[str, PackageSpec], active_names: set[str]
) -> Iterator[tuple[Path, Path]]:
    """Yield (link_path, repo_target) for every managed symlink under `target`.

    This is the expensive part of both `--apply` and `--check` (cli.py times
    it). Sharing one traversal keeps the two modes from disagreeing about
    which links exist. Callers get the pair and decide what it means: prune
    tests ignore-patterns, check tests staleness.

    Each scan root is read one level deep, because collect_scan_roots already
    enumerates every mirrored directory -- recursing would re-walk unmanaged
    trees that happen to sit under a mirrored path. Roots arrive parent-first,
    so the prune step's parent walk still sees containers before contents.
    """
    script_real = REPO_ROOT.resolve()
    scan_roots = collect_scan_roots(specs, target, active_names)
    mirrored = set(scan_roots)
    seen: set[Path] = set()
    orphan_roots: list[Path] = []
    for scan_root in scan_roots:
        if scan_root.is_symlink():
            candidates: Iterable[Path] = (scan_root,)
        else:
            try:
                entries = sorted(scan_root.iterdir())
            except OSError:
                # Not yet linked, or unreadable. Nothing to audit either way.
                continue
            candidates = chain((scan_root,), entries)
            orphan_roots.extend(
                item
                for item in entries
                if item.is_dir() and not item.is_symlink() and item not in mirrored
            )

        for path in candidates:
            if path in seen or not path.is_symlink():
                continue
            seen.add(path)
            repo_target = managed_link_target(path, script_real)
            if repo_target is not None:
                yield path, repo_target

    # Directories the repo used to mirror, walked after the mirrored set so
    # containers are still seen before their contents.
    for orphan_root in orphan_roots:
        for path, repo_target in iter_orphaned_managed_links(
            orphan_root, mirrored, script_real
        ):
            if path in seen:
                continue
            seen.add(path)
            yield path, repo_target


def prune_stale_managed_links(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
) -> None:
    """Remove managed links whose repo source is gone.

    `--check` reports these as STALE, but nothing used to clear them:
    run_apply_group only fixes links that are still in a package's plan, and a
    file deleted from the repo produces no plan entry. So deleting a script
    left its symlink in `$HOME` forever, dangling, and every later `--check`
    reported an issue that no `--apply` could resolve.

    Only broken links are pruned. A dangling symlink into this repo is garbage
    whoever owns it, whereas a link that still resolves may belong to a package
    that is merely inactive on this host -- that is the INVALID case, which is
    reported and left alone.
    """
    print_header = lazy_header("stale-symlinks")

    for path, _repo_target in iter_managed_links(target, specs, active_names):
        if path.exists():
            continue
        try:
            path.unlink()
        except OSError:
            continue
        print_header()
        LOGGER.warning(f"CLEARED STALE: {path.relative_to(target)}")


def prune_managed_ignored_artifact_links(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    verbose: bool,
) -> None:
    """Remove stale managed links for repo-ignored build/cache artifacts."""
    rules = IgnoreRules.load()

    linked_paths: list[Path] = []
    ignored_dirs: set[Path] = set()

    for path, repo_target in iter_managed_links(target, specs, active_names):
        if not repo_path_is_ignored(repo_target, specs, rules):
            continue
        linked_paths.append(path)
        parents_to_prune: list[Path] = []
        for target_parent, repo_parent in zip(
            path.parents, repo_target.parents, strict=False
        ):
            if target_parent == target:
                break
            parents_to_prune.append(target_parent)
            if repo_path_is_ignored(repo_parent, specs, rules):
                ignored_dirs.update(parents_to_prune)
                break

    if not linked_paths and not ignored_dirs:
        return

    print_header = lazy_header("ignored-artifacts")

    for path in sorted(linked_paths, key=lambda item: len(item.parts), reverse=True):
        try:
            path.unlink()
        except FileNotFoundError:
            continue
        print_header()
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
        print_header()
        LOGGER.warning(f"REMOVED: {path.relative_to(target)}")


def check_repo_backlinks(
    target: Path,
    specs: dict[str, PackageSpec],
    active_names: set[str],
    *,
    ignore: set[str],
) -> bool:
    stale_header = lazy_header("stale-symlinks")
    invalid_header = lazy_header("invalid-backlinks")
    has_issues = False

    for path, repo_target in iter_managed_links(target, specs, active_names):
        spec = owner_for_repo_path(repo_target, specs)
        if spec is None:
            continue

        rel_path = path.relative_to(target)
        if not path.exists():
            issue_id = f"stale:{rel_path}"
            if issue_id in ignore:
                continue
            stale_header()
            has_issues = True
            LOGGER.warning(f"STALE: {rel_path}  (--ignore {issue_id})")
            continue

        if spec.name not in active_names:
            issue_id = f"invalid:{rel_path}"
            if issue_id in ignore:
                continue
            invalid_header()
            has_issues = True
            LOGGER.warning(f"INVALID: {rel_path} [{spec.scope}]  (--ignore {issue_id})")

    return has_issues
