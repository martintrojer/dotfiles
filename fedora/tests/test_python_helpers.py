#!/usr/bin/env python3
"""Focused behavior tests for Fedora Python helpers."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import os
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
STEAM_PAUSE = ROOT / "fedora/gaming/home/.local/bin/steam-pause"


def load_script(name: str, path: Path) -> types.ModuleType:
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


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

    def test_steam_pause_parses_proc_tree_and_signals_only_descendants(self) -> None:
        steam_pause = cast(Any, load_script("steam_pause_test", STEAM_PAUSE))
        with tempfile.TemporaryDirectory() as raw_tmp:
            proc = Path(raw_tmp)
            processes = {
                100: (1, "reaper", b"/usr/bin/reaper\0SteamLaunch\0AppId=42\0--\0"),
                101: (100, "game worker", b"game\0"),
                102: (101, "child ) worker", b"child\0"),
                200: (1, "unrelated", b"other\0"),
            }
            for pid, (ppid, name, cmdline) in processes.items():
                pid_dir = proc / str(pid)
                pid_dir.mkdir()
                (pid_dir / "cmdline").write_bytes(cmdline)
                (pid_dir / "stat").write_text(f"{pid} ({name}) S {ppid} 0 0 0\n")
            steam_pause.PROC = proc

            self.assertEqual(steam_pause.game_reapers(), [(100, 42)])
            kids = steam_pause.children_map()
            self.assertEqual(steam_pause.descendants(100, kids), [101, 102])
            with mock.patch.object(steam_pause.os, "kill") as kill:
                count = steam_pause.signal_tree(100, signal.SIGSTOP, kids)

            self.assertEqual(count, 2)
            self.assertEqual(
                kill.call_args_list,
                [mock.call(101, signal.SIGSTOP), mock.call(102, signal.SIGSTOP)],
            )


if __name__ == "__main__":
    unittest.main()
