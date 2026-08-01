#!/usr/bin/env python3
"""Focused behavior tests for Fedora Python helpers."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import re
import shutil
import signal
import subprocess
import sys
import tempfile
import types
import unittest
from pathlib import Path
from typing import Any, cast
from unittest import mock

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


def install_command(wrapper: Path, marker: str) -> str:
    lines = [
        line
        for line in wrapper.read_text().splitlines()
        if not line.lstrip().startswith("#") and marker in line
    ]
    assert len(lines) == 1, f"{wrapper.name}: expected 1 install line, got {lines}"
    return lines[0]


def read_array(script: Path) -> list[str]:
    name = re.match(r"(\w+?)-packages\.sh", script.name)
    assert name, script.name
    var = f"{name.group(1)}_packages"
    out = subprocess.run(
        [
            "bash",
            "-c",
            f'source "$1"; printf "%s\\n" "${{{var}[@]}}"',
            "_",
            str(script),
        ],
        capture_output=True,
        text=True,
        check=True,
    )
    return [line for line in out.stdout.split("\n") if line]


def load_script(name: str, path: Path) -> types.ModuleType:
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


class FedoraPackageArrays(unittest.TestCase):
    def test_arrays_are_non_empty_and_unique(self) -> None:
        for array_script in dict.fromkeys(setup[0] for setup in FEDORA_SETUPS):
            with self.subTest(script=array_script.name):
                packages = read_array(array_script)
                self.assertTrue(packages, f"{array_script.name} exports no packages")
                self.assertCountEqual(
                    packages,
                    set(packages),
                    f"{array_script.name} lists a package twice",
                )

    def test_wrappers_install_the_sourced_array(self) -> None:
        for array_script, wrapper, marker in FEDORA_SETUPS:
            with self.subTest(script=wrapper.name):
                var = f"{array_script.name.split('-')[0]}_packages"
                self.assertIn(f'"${{{var}[@]}}"', install_command(wrapper, marker))

    def test_scripts_are_syntactically_valid(self) -> None:
        for array_script, wrapper, _ in FEDORA_SETUPS:
            for script in (array_script, wrapper):
                with self.subTest(script=script.name):
                    subprocess.run(["bash", "-n", str(script)], check=True)


class FedoraStowGroupTests(unittest.TestCase):
    """The fedora/gaming groups nest their stow_dir below the repo root, so the
    package name is not at a fixed offset in the source path stow prints."""

    def test_ignored_conflict_maps_to_the_owning_package(self) -> None:
        sys.path.insert(0, str(ROOT))
        from _dotfiles_sync.stow import package_for_conflict

        target = ROOT.parent
        cases = [
            (ROOT, "zsh/.zshrc", "zsh"),
            (ROOT / "fedora", "bin/.local/bin/cava", "bin"),
            (
                ROOT / "fedora/gaming",
                "home/.config/MangoHud/MangoHud.conf",
                "home",
            ),
        ]
        for stow_dir, tail, expected in cases:
            source_rel = f"{os.path.relpath(stow_dir, target)}/{tail}"
            with self.subTest(stow_dir=stow_dir.name):
                self.assertEqual(
                    package_for_conflict(source_rel, stow_dir, target), expected
                )

    def test_conflict_outside_the_group_maps_to_nothing(self) -> None:
        sys.path.insert(0, str(ROOT))
        from _dotfiles_sync.stow import package_for_conflict

        self.assertIsNone(
            package_for_conflict("elsewhere/bin/tool", ROOT / "fedora", ROOT.parent)
        )


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
