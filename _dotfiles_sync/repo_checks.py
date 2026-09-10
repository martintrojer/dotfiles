from __future__ import annotations

import logging
import os
import re
import subprocess
from pathlib import Path

from .config import REPO_ROOT, REPO_WALK_SKIP_DIRS, lazy_header
from .inventory import IGNORED_TOPLEVEL_DIRS
from .model import PackageSpec

LOGGER = logging.getLogger("dotfiles-sync")

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
    for child in sorted(REPO_ROOT.iterdir(), key=lambda path: path.name):
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


def repo_rel(path: Path) -> str:
    return path.relative_to(REPO_ROOT).as_posix()


def _git_tracked_or_committable() -> list[Path] | None:
    """Repo files git would let you commit today, or None if git can't say.

    ``--cached --others --exclude-standard`` is "tracked, plus untracked but
    not ignored" -- precisely the set the private-env scan cares about. A
    gitignored file was never committable, so flagging it only costs an
    ``--ignore`` entry that never expires.
    """
    result = subprocess.run(
        ["git", "ls-files", "-z", "--cached", "--others", "--exclude-standard"],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=False,
    )
    if result.returncode != 0:
        return None
    return [REPO_ROOT / rel for rel in result.stdout.split("\0") if rel and rel.strip()]


def iter_repo_files() -> list[Path]:
    from_git = _git_tracked_or_committable()
    if from_git is not None:
        # git lists deleted-but-staged paths and symlinks too; keep the walk's
        # contract of "real files present on disk".
        return [path for path in from_git if path.is_file() and not path.is_symlink()]
    # No usable git (e.g. a bare jj workspace): fall back to walking the tree
    # with the hand-maintained skip list.
    files: list[Path] = []
    for root, dirnames, filenames in os.walk(REPO_ROOT):
        root_path = Path(root)
        dirnames[:] = [
            dirname
            for dirname in dirnames
            if dirname not in REPO_WALK_SKIP_DIRS
            and f"{repo_rel(root_path / dirname)}" not in REPO_WALK_SKIP_DIRS
        ]
        for filename in filenames:
            path = root_path / filename
            if path.is_symlink():
                continue
            files.append(path)
    return files


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

    for path in sorted(iter_repo_files()):
        if path.suffix != ".service":
            continue
        rel_unit = repo_rel(path)
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

        for extra_rel in EXTRA_UNIT_TARGETS.get(rel_unit, ()):
            if (REPO_ROOT / extra_rel).exists():
                LOGGER.info(f"  ok  {rel_unit}: {extra_rel}")
                continue
            issue_id = f"unit-target:{rel_unit}:{extra_rel}"
            if issue_id in ignore:
                continue
            warn(
                f"UNIT-TARGET: {rel_unit} is installed from {extra_rel}, which is "
                f"missing  (--ignore {issue_id})"
            )

    return found_issue
