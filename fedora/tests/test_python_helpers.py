#!/usr/bin/env python3
"""Focused behavior tests for Fedora Python helpers."""

from __future__ import annotations

import argparse
import contextlib
import io
import json
import logging
import os
import shutil
import signal
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from typing import Any, cast
from unittest import mock

sys.path.insert(0, str(Path(__file__).parent))

# Imported after the sys.path line above.
import _package_harness as harness

load_script = harness.load_script

ROOT = Path(__file__).parents[2]
WALLPAPER = ROOT / "fedora/bin/.local/bin/wallpaper"
TBX = ROOT / "fedora/bin/.local/bin/tbx"
LMSTUDIO = ROOT / "fedora/bin/.local/bin/lmstudio-server"
# (package array script, wrapper script, install command the wrapper uses)
FEDORA_SETUPS = [
    (
        ROOT / "fedora/os/base-packages.sh",
        ROOT / "fedora/os/setup-base.sh",
        "rpm-ostree install",
    ),
    (
        ROOT / "fedora/os/sway-packages.sh",
        ROOT / "fedora/os/setup-sway.sh",
        "rpm-ostree install",
    ),
    # Inside a toolbox there is no rpm-ostree; the same array is layered with dnf.
    (
        ROOT / "fedora/os/base-packages.sh",
        ROOT / "fedora/os/setup-toolbox.sh",
        "dnf install",
    ),
]


class FedoraPackageArrays(harness.PackageArrayChecks):
    SETUPS = FEDORA_SETUPS


class PackageOwnershipTests(unittest.TestCase):
    """The fedora/gaming groups nest their package dir below the repo root.

    The planner carries the owning package on every Link, so the old
    reverse-mapping from a printed source path (package_for_conflict) is gone.
    This pins the property that replaced it: a link knows its package
    regardless of how deep the group sits.
    """

    def test_links_report_their_owning_package(self) -> None:
        sys.path.insert(0, str(ROOT))
        from _dotfiles_sync.inventory import build_specs
        from _dotfiles_sync.sync import plan_group

        specs = build_specs()
        for name in ("bin", "home", "zsh"):
            with self.subTest(package=name):
                links = plan_group([name], specs, ROOT / "nonexistent-target")
                self.assertTrue(links, f"{name} planned no links")
                self.assertEqual({link.package for link in links}, {name})


class SystemdUnitTargetTests(unittest.TestCase):
    """Units live in the `systemd` package but point at scripts in `sway`,
    `waybar` and `fedora/bin`. Nothing else in the gate notices when one of
    those moves; the failure would otherwise surface at next login."""

    def setUp(self) -> None:
        sys.path.insert(0, str(ROOT))
        from _dotfiles_sync.inventory import build_specs
        from _dotfiles_sync.repo_checks import check_systemd_unit_targets

        self.check = check_systemd_unit_targets
        self.specs = build_specs()
        # The negative cases warn by design; keep the test output readable.
        logger = logging.getLogger("dotfiles-sync")
        previous = logger.disabled
        logger.disabled = True
        self.addCleanup(setattr, logger, "disabled", previous)

    def test_every_shipped_unit_target_resolves_today(self) -> None:
        self.assertFalse(self.check(self.specs, ignore=set()))

    def test_a_moved_execstart_target_is_caught(self) -> None:
        target = ROOT / "sway/.config/sway/scripts/session-wallpaper"
        moved = target.with_name(target.name + ".moved-by-test")
        target.rename(moved)
        try:
            self.assertTrue(self.check(self.specs, ignore=set()))
        finally:
            moved.rename(target)

    def test_a_missing_install_source_is_caught(self) -> None:
        # steam-pause reaches its unit via a sudo-install copy in /usr/local/bin,
        # so the check asserts the repo-side source instead.
        target = ROOT / "fedora/gaming/home/.local/bin/steam-pause"
        moved = target.with_name(target.name + ".moved-by-test")
        target.rename(moved)
        try:
            self.assertTrue(self.check(self.specs, ignore=set()))
        finally:
            moved.rename(target)


class FedoraPythonHelperTests(unittest.TestCase):
    def test_wallpaper_archives_symlink_target_without_moving_source(self) -> None:
        wallpaper = cast(Any, load_script("wallpaper_test", WALLPAPER))
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            source = root / "outside/source image.png"
            source.parent.mkdir()
            source.write_bytes(b"wallpaper")
            wallpaper.ARCHIVE_DIR = root / "wallpapers/archive"
            wallpaper.ARCHIVE_DIR.mkdir(parents=True)
            wallpaper.CURRENT_LINK = root / "wallpapers/current"
            wallpaper.CURRENT_LINK.symlink_to(source)

            wallpaper.archive_existing_current()

            archived = list(wallpaper.ARCHIVE_DIR.iterdir())
            self.assertEqual(len(archived), 1)
            self.assertEqual(archived[0].read_bytes(), b"wallpaper")
            self.assertEqual(wallpaper.CURRENT_LINK.resolve(), source)
            self.assertEqual(source.read_bytes(), b"wallpaper")

    def wallpaper_env(self, root: Path) -> Any:
        """Load wallpaper with its data/cache dirs pointed at a scratch tree."""
        wallpaper = cast(Any, load_script("wallpaper_test", WALLPAPER))
        wallpaper.TARGET_DIR = root / "wallpapers"
        wallpaper.ARCHIVE_DIR = wallpaper.TARGET_DIR / "archive"
        wallpaper.CURRENT_LINK = wallpaper.TARGET_DIR / "current"
        wallpaper.CACHE_DIR = root / "cache"
        wallpaper.ARCHIVE_DIR.mkdir(parents=True)
        wallpaper.CACHE_DIR.mkdir()
        return wallpaper

    def test_status_json_is_the_contract_lock_screen_parses(self) -> None:
        """sway's lock-screen falls back to a solid colour whenever `wallpaper
        status` exits nonzero or prints something it can't parse, so a
        regression here degrades locking silently.
        """
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            wallpaper = self.wallpaper_env(root)
            args = argparse.Namespace()

            with contextlib.redirect_stdout(io.StringIO()) as out:
                self.assertEqual(wallpaper.cmd_status(args), 1)
            self.assertEqual(json.loads(out.getvalue()), None)

            image = wallpaper.ARCHIVE_DIR / "20240101-000000-pic.abcdef.png"
            image.write_bytes(b"not really a png")
            wallpaper.CURRENT_LINK.symlink_to(image)

            # No cache rendered yet: status still succeeds, lock_image is null.
            with contextlib.redirect_stdout(io.StringIO()) as out:
                self.assertEqual(wallpaper.cmd_status(args), 0)
            self.assertEqual(
                json.loads(out.getvalue()), {"path": str(image), "lock_image": None}
            )

            # An empty cache file must not be advertised as a lock image.
            cache = wallpaper._cache_path(image)
            cache.touch()
            with contextlib.redirect_stdout(io.StringIO()) as out:
                wallpaper.cmd_status(args)
            self.assertIsNone(json.loads(out.getvalue())["lock_image"])

            cache.write_bytes(b"rendered")
            with contextlib.redirect_stdout(io.StringIO()) as out:
                self.assertEqual(wallpaper.cmd_status(args), 0)
            self.assertEqual(
                json.loads(out.getvalue()),
                {"path": str(image), "lock_image": str(cache)},
            )

    def test_failed_rebuild_keeps_the_previous_lock_image(self) -> None:
        """A forced rebuild that fails must not destroy the working cache --
        that would leave the next lock screen bare until magick works again.
        """
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            wallpaper = self.wallpaper_env(root)
            image = wallpaper.ARCHIVE_DIR / "pic.png"
            image.write_bytes(b"source")
            cached = wallpaper._cache_path(image)
            cached.write_bytes(b"previous good render")

            with mock.patch.object(
                wallpaper, "_build_lock_image", return_value=(False, "magick exploded")
            ):
                built, error = wallpaper.build_lock_cache(image, force=True)

            self.assertIsNone(built)
            self.assertEqual(error, "magick exploded")
            self.assertEqual(cached.read_bytes(), b"previous good render")

    def test_cache_key_tracks_render_version_and_source_mtime(self) -> None:
        """Stale keys mean a lock screen showing a wallpaper you replaced."""
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            wallpaper = self.wallpaper_env(root)
            image = wallpaper.ARCHIVE_DIR / "pic.png"
            image.write_bytes(b"source")

            key = wallpaper._cache_key(image)
            self.assertEqual(key, wallpaper._cache_key(image))

            os.utime(image, ns=(0, 1234567890))
            touched = wallpaper._cache_key(image)
            self.assertNotEqual(touched, key)

            wallpaper.RENDER_VERSION += 1
            self.assertNotEqual(wallpaper._cache_key(image), touched)

            self.assertIsNone(wallpaper._cache_key(wallpaper.ARCHIVE_DIR / "gone.png"))

    def test_delete_archive_clears_cache_but_spares_the_active_wallpaper(self) -> None:
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            wallpaper = self.wallpaper_env(root)
            active = wallpaper.ARCHIVE_DIR / "active.png"
            stale = wallpaper.ARCHIVE_DIR / "stale.png"
            active.write_bytes(b"active")
            stale.write_bytes(b"stale")
            wallpaper.CURRENT_LINK.symlink_to(active)

            active_cache = wallpaper._cache_path(active)
            active_cache.write_bytes(b"png")
            key = wallpaper._cache_key(stale)
            for name in (f"{key}.png", f"{key}.sixel-80x24", f"{key}.sixel-120x40"):
                (wallpaper.CACHE_DIR / name).write_bytes(b"artifact")
            unrelated = wallpaper.CACHE_DIR / "deadbeef.png"
            unrelated.write_bytes(b"someone else's")

            with contextlib.redirect_stderr(io.StringIO()) as err:
                self.assertEqual(
                    wallpaper.cmd__delete_archive(
                        argparse.Namespace(basename="active.png")
                    ),
                    0,
                )
            self.assertIn("refusing to delete currently-active", err.getvalue())
            self.assertTrue(active.is_file())
            self.assertTrue(active_cache.is_file())

            self.assertEqual(
                wallpaper.cmd__delete_archive(argparse.Namespace(basename="stale.png")),
                0,
            )
            self.assertFalse(stale.exists())
            self.assertEqual(
                sorted(p.name for p in wallpaper.CACHE_DIR.iterdir()),
                sorted([active_cache.name, unrelated.name]),
            )

    def test_tbx_host_lookup_skips_its_wrapper_directory(self) -> None:
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            wrappers = root / "wrappers"
            real_bin = root / "real-bin"
            wrappers.mkdir()
            real_bin.mkdir()
            tbx = wrappers / "tbx"
            shutil.copy2(TBX, tbx)
            wrapper = wrappers / "demo-tool"
            wrapper.write_text("#!/bin/sh\nprintf 'wrapper\\n'\n")
            wrapper.chmod(0o755)
            real = real_bin / "demo-tool"
            real.write_text("#!/bin/sh\nprintf 'real:%s\\n' \"$*\"\n")
            real.chmod(0o755)

            result = subprocess.run(
                [sys.executable, str(tbx), "--prefer-host", "demo-tool", "argument"],
                env={
                    **os.environ,
                    "HOME": str(root / "home"),
                    "PATH": os.pathsep.join(
                        (str(wrappers), str(real_bin), os.environ["PATH"])
                    ),
                },
                check=False,
                capture_output=True,
                text=True,
                timeout=5,
            )

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout, "real:argument\n")

    def test_lmstudio_signal_deaths_map_to_conventional_exit_codes(self) -> None:
        lmstudio = cast(Any, load_script("lmstudio_server_test", LMSTUDIO))
        # A stop we asked for is a clean shutdown, whatever Popen reports.
        self.assertEqual(lmstudio.exit_status(-signal.SIGTERM, True), 0)
        # An unrequested signal death reports 128+signum, not a negative
        # number: sys.exit(-15) reaches systemd as 241.
        self.assertEqual(lmstudio.exit_status(-signal.SIGTERM, False), 143)
        self.assertEqual(lmstudio.exit_status(-signal.SIGKILL, False), 137)
        # A crash still fails, so Restart=on-failure keeps working.
        self.assertEqual(lmstudio.exit_status(1, False), 1)
        self.assertEqual(lmstudio.exit_status(0, False), 1)
        self.assertEqual(lmstudio.exit_status(None, False), 1)

    def test_lmstudio_exits_zero_when_terminated_while_supervising(self) -> None:
        lmstudio = cast(Any, load_script("lmstudio_server_test", LMSTUDIO))
        handlers: dict[int, Any] = {}
        child = subprocess.Popen(["sleep", "30"])
        self.addCleanup(child.wait)
        self.addCleanup(child.kill)

        def sleep(_seconds: float) -> None:
            # Stand in for systemd delivering SIGTERM mid-supervision.
            handlers[signal.SIGTERM](signal.SIGTERM, None)
            child.wait(timeout=5)

        with (
            mock.patch.object(lmstudio, "wait_for_wayland", return_value=True),
            mock.patch.object(lmstudio, "wait_for_daemon", return_value=True),
            # False once so main() does not refuse an already-running API,
            # then healthy for the readiness and supervision loops.
            mock.patch.object(lmstudio, "api_ready", side_effect=[False] + [True] * 8),
            mock.patch.object(lmstudio, "notify"),
            mock.patch.object(
                lmstudio,
                "lms",
                return_value=subprocess.CompletedProcess([], 0, "", ""),
            ),
            mock.patch.object(
                lmstudio.signal,
                "signal",
                side_effect=lambda num, handler: handlers.__setitem__(num, handler),
            ),
            mock.patch.object(lmstudio.subprocess, "Popen", return_value=child),
            mock.patch.object(lmstudio.time, "sleep", side_effect=sleep),
        ):
            status = lmstudio.main()

        self.assertEqual(child.returncode, -signal.SIGTERM)
        self.assertEqual(status, 0)


if __name__ == "__main__":
    unittest.main()
