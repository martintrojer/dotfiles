#!/usr/bin/env python3
"""Behavior tests for gaming package setup and Python helpers."""

from __future__ import annotations

import importlib.machinery
import importlib.util
import re
import signal
import subprocess
import sys
import tempfile
import types
import unittest
from pathlib import Path
from typing import Any, cast
from unittest import mock

FEDORA = Path(__file__).parents[2]
STEAM_PAUSE = FEDORA / "gaming/home/.local/bin/steam-pause"

# (package array script, wrapper script)
SETUPS = [
    (FEDORA / "gaming/os/steam-packages.sh", FEDORA / "gaming/os/setup-steam.sh"),
]

# Shipped by the Sericea base image, so deliberately absent from the arrays:
# listing one makes `rpm-ostree install` fail with "already provided by" and
# layer nothing at all. See the header of steam-packages.sh.
BASE_IMAGE_PACKAGES = {"gamemode", "7zip"}


def install_command(wrapper: Path) -> str:
    """The actual `rpm-ostree install` line, with comments stripped.

    Matching against the whole file would let an explanatory comment satisfy
    the assertion while the real command differs.
    """
    lines = [
        line
        for line in wrapper.read_text().splitlines()
        if not line.lstrip().startswith("#") and "rpm-ostree install" in line
    ]
    assert len(lines) == 1, f"{wrapper.name}: expected 1 install line, got {lines}"
    return lines[0]


def load_script(name: str, path: Path) -> types.ModuleType:
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


def read_array(script: Path) -> list[str]:
    """Source the array script and echo its one array, exactly as setup does."""
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


class PackageArrays(unittest.TestCase):
    def test_arrays_are_non_empty_and_unique(self):
        for array_script, _ in SETUPS:
            with self.subTest(script=array_script.name):
                packages = read_array(array_script)
                self.assertTrue(packages, f"{array_script.name} exports no packages")
                self.assertCountEqual(
                    packages,
                    set(packages),
                    f"{array_script.name} lists a package twice",
                )

    def test_arrays_omit_base_image_packages(self):
        """A base-image package in the array makes rpm-ostree layer nothing."""
        for array_script, _ in SETUPS:
            with self.subTest(script=array_script.name):
                listed = set(read_array(array_script)) & BASE_IMAGE_PACKAGES
                self.assertFalse(
                    listed,
                    f"{array_script.name} lists base-image package(s) "
                    f"{sorted(listed)}; rpm-ostree will fail with 'already "
                    f"provided by' and layer none of the array. Drop them, or "
                    f"add --allow-inactive to the wrapper.",
                )

    def test_wrappers_install_the_sourced_array(self):
        for array_script, wrapper in SETUPS:
            with self.subTest(script=wrapper.name):
                var = f"{array_script.name.split('-')[0]}_packages"
                self.assertIn(f'"${{{var}[@]}}"', install_command(wrapper))

    def test_scripts_are_syntactically_valid(self):
        for array_script, wrapper in SETUPS:
            for script in (array_script, wrapper):
                with self.subTest(script=script.name):
                    subprocess.run(["bash", "-n", str(script)], check=True)


class GamingPythonHelperTests(unittest.TestCase):
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

    def test_steam_pause_reports_paused_only_when_whole_tree_is_stopped(self) -> None:
        steam_pause = cast(Any, load_script("steam_pause_test", STEAM_PAUSE))
        with tempfile.TemporaryDirectory() as raw_tmp:
            proc = Path(raw_tmp)
            steam_pause.PROC = proc

            def set_states(states: dict[int, str]) -> None:
                for pid, state in states.items():
                    pid_dir = proc / str(pid)
                    pid_dir.mkdir(exist_ok=True)
                    (pid_dir / "stat").write_text(f"{pid} (game ) x) {state} 100 0 0\n")

            set_states({101: "R", 102: "T"})
            # Order must not matter: descendants() pops off a stack, so which
            # pid comes first is arbitrary between runs.
            self.assertFalse(steam_pause.is_paused([101, 102]))
            self.assertFalse(steam_pause.is_paused([102, 101]))

            set_states({101: "T", 102: "T"})
            self.assertTrue(steam_pause.is_paused([101, 102]))

            # Vanished pids are ignored, but an entirely gone tree is not paused.
            self.assertTrue(steam_pause.is_paused([101, 102, 999]))
            self.assertFalse(steam_pause.is_paused([999]))
            self.assertFalse(steam_pause.is_paused([]))


if __name__ == "__main__":
    unittest.main()
