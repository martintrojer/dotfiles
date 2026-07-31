#!/usr/bin/env python3
"""Focused behavior tests for Fedora Python helpers."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
import re
import shutil
import subprocess
import sys
import tempfile
import types
import unittest
from pathlib import Path
from typing import Any, cast

ROOT = Path(__file__).parents[2]
WALLPAPER = ROOT / "fedora/bin/.local/bin/wallpaper"
TBX = ROOT / "fedora/bin/.local/bin/tbx"
FEDORA_SETUPS = [
    (ROOT / "fedora/os/base-packages.sh", ROOT / "fedora/os/setup-base.sh"),
    (ROOT / "fedora/os/sway-packages.sh", ROOT / "fedora/os/setup-sway.sh"),
]


def install_command(wrapper: Path) -> str:
    lines = [
        line
        for line in wrapper.read_text().splitlines()
        if not line.lstrip().startswith("#") and "rpm-ostree install" in line
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
        for array_script, _ in FEDORA_SETUPS:
            with self.subTest(script=array_script.name):
                packages = read_array(array_script)
                self.assertTrue(packages, f"{array_script.name} exports no packages")
                self.assertCountEqual(
                    packages,
                    set(packages),
                    f"{array_script.name} lists a package twice",
                )

    def test_wrappers_install_the_sourced_array(self) -> None:
        for array_script, wrapper in FEDORA_SETUPS:
            with self.subTest(script=wrapper.name):
                var = f"{array_script.name.split('-')[0]}_packages"
                self.assertIn(f'"${{{var}[@]}}"', install_command(wrapper))

    def test_scripts_are_syntactically_valid(self) -> None:
        for array_script, wrapper in FEDORA_SETUPS:
            for script in (array_script, wrapper):
                with self.subTest(script=script.name):
                    subprocess.run(["bash", "-n", str(script)], check=True)


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


if __name__ == "__main__":
    unittest.main()
