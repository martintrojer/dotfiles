from __future__ import annotations

import ipaddress
import logging
import os
import re
import subprocess
from collections.abc import Iterable, Iterator
from itertools import chain
from pathlib import Path
from typing import Final
from urllib.parse import urlparse

from .config import SCRIPT_DIR, lazy_header
from .ignore import IgnoreRules
from .inventory import IGNORED_TOPLEVEL_DIRS
from .model import PackageSpec

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
# Fallback skip list, used only when the repo cannot be listed via git (see
# _iter_repo_files). Prefer .gitignore: a path git ignores is a path that
# cannot be committed, which is exactly what these scans are about.
REPO_SCAN_SKIP_DIRS = {
    ".direnv",
    ".git",
    ".jj",
    ".mypy_cache",
    ".nox",
    ".pytest_cache",
    ".ruff_cache",
    ".tox",
    ".venv",
    "__pycache__",
    "build",
    "dist",
    "guides/build",
    "node_modules",
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
EXEC_DIRECTIVE_RE = re.compile(r"^\s*Exec(?:Start|StartPre|StartPost|Stop|Reload)\s*=")
# Units live in the `systemd` package but their ExecStart targets live in other
# packages (sway, waybar, fedora/bin). Rename one of those scripts and every
# gate stays green while the next login comes up with no bar and no wallpaper.
# Paths that are not %h-relative (/usr/bin/waybar, /usr/bin/kanshi) belong to
# the distro, not this repo, so they are skipped. /usr/local/bin copies made by
# a setup script are covered by EXTRA_UNIT_TARGETS instead.
UNIT_HOME_PATH_RE = re.compile(r"%h(/[^\s'\"]+)")
# ExecStart targets this repo owns but cannot reach through %h, because a setup
# script installs a copy outside $HOME. Asserting the *source* exists is the
# closest static check available. Maps a unit path to the repo paths it needs.
EXTRA_UNIT_TARGETS: dict[str, tuple[str, ...]] = {
    # setup-steam-pause.sh installs ~/.local/bin/steam-pause (from the gaming
    # `home` package) to /usr/local/bin/steam-pause, where the unit finds it.
    "fedora/gaming/config/systemd-system/steam-pause-games.service": (
        "fedora/gaming/home/.local/bin/steam-pause",
    ),
}


def check_package_coverage(specs: dict[str, PackageSpec], *, ignore: set[str]) -> bool:
    expected = set(specs) | IGNORED_TOPLEVEL_DIRS
    print_header = lazy_header("package-coverage")
    found_issue = False
    for child in sorted(SCRIPT_DIR.iterdir(), key=lambda path: path.name):
        if not child.is_dir() or child.name.startswith("."):
            continue
        if child.name not in expected:
            issue_id = f"unclassified:{child.name}"
            if issue_id in ignore:
                continue
            print_header()
            found_issue = True
            LOGGER.warning(f"UNCLASSIFIED: {child.name}  (--ignore {issue_id})")
    return found_issue


def _repo_rel(path: Path) -> str:
    return path.relative_to(SCRIPT_DIR).as_posix()


def _git_tracked_or_committable() -> list[Path] | None:
    """Repo files git would let you commit today, or None if git can't say.

    ``--cached --others --exclude-standard`` is "tracked, plus untracked but
    not ignored" -- precisely the set the private-env scan cares about. A
    gitignored file was never committable, so flagging it only costs an
    ``--ignore`` entry that never expires.
    """
    result = subprocess.run(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
        cwd=SCRIPT_DIR,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    return [
        SCRIPT_DIR / rel for rel in result.stdout.split("\0") if rel and rel.strip()
    ]


def _iter_repo_files() -> list[Path]:
    from_git = _git_tracked_or_committable()
    if from_git is not None:
        # git lists deleted-but-staged paths and symlinks too; keep the walk's
        # contract of "real files present on disk".
        return [path for path in from_git if path.is_file() and not path.is_symlink()]
    # No usable git (e.g. a bare jj workspace): fall back to walking the tree
    # with the hand-maintained skip list.
    files: list[Path] = []
    for root, dirnames, filenames in os.walk(SCRIPT_DIR):
        root_path = Path(root)
        dirnames[:] = [
            dirname
            for dirname in dirnames
            if dirname not in REPO_SCAN_SKIP_DIRS
            and f"{_repo_rel(root_path / dirname)}" not in REPO_SCAN_SKIP_DIRS
        ]
        for filename in filenames:
            path = root_path / filename
            if path.is_symlink():
                continue
            files.append(path)
    return files


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

    for path in _iter_repo_files():
        rel_path = _repo_rel(path)
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


def check_systemd_unit_targets(
    specs: dict[str, PackageSpec], *, ignore: set[str]
) -> bool:
    """Assert every %h path in a shipped unit resolves inside a stow package.

    Pure filesystem, no systemd needed, so it runs on macOS and in CI. The
    owning package is reported so cross-scope references stay visible: units
    are scope "fedora" while sway/waybar are scope "linux".
    """
    print_header = lazy_header("systemd-unit-targets")
    found_issue = False

    def warn(message: str) -> None:
        nonlocal found_issue
        print_header()
        found_issue = True
        LOGGER.warning(message)

    # %h expands to the user's home, and every stow package mirrors $HOME, so a
    # %h path resolves by trying it under each package root.
    def owner_of(home_rel: str) -> PackageSpec | None:
        for spec in specs.values():
            if (spec.package_dir / home_rel).exists():
                return spec
        return None

    for path in sorted(_iter_repo_files()):
        if path.suffix != ".service":
            continue
        rel_unit = _repo_rel(path)
        try:
            lines = path.read_text(errors="replace").splitlines()
        except OSError:
            continue

        for lineno, line in enumerate(lines, start=1):
            if not EXEC_DIRECTIVE_RE.match(line):
                continue
            for match in UNIT_HOME_PATH_RE.finditer(line):
                home_rel = match.group(1).lstrip("/")
                spec = owner_of(home_rel)
                if spec is not None:
                    LOGGER.info(f"  ok  {rel_unit}: %h/{home_rel} [{spec.name}]")
                    continue
                issue_id = f"unit-target:{rel_unit}:{home_rel}"
                if issue_id in ignore:
                    continue
                warn(
                    f"UNIT-TARGET: {rel_unit}:{lineno} needs %h/{home_rel}, which "
                    f"no stow package provides  (--ignore {issue_id})"
                )

        for repo_rel in EXTRA_UNIT_TARGETS.get(rel_unit, ()):
            if (SCRIPT_DIR / repo_rel).exists():
                LOGGER.info(f"  ok  {rel_unit}: {repo_rel}")
                continue
            issue_id = f"unit-target:{rel_unit}:{repo_rel}"
            if issue_id in ignore:
                continue
            warn(
                f"UNIT-TARGET: {rel_unit} is installed from {repo_rel}, which is "
                f"missing  (--ignore {issue_id})"
            )

    return found_issue


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


# How far below a mirrored directory the orphan scan will descend. The repo's
# own deleted shapes are shallow (a systemd `*.service.d/` drop-in is one
# level), and this is the only thing making the scan's cost independent of
# what happens to sit under a mirrored path -- see iter_orphaned_managed_links.
MAX_ORPHAN_DEPTH: Final[int] = 3


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
    script_real = SCRIPT_DIR.resolve()
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
