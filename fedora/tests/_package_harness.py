#!/usr/bin/env python3
"""Shared harness for the fedora and fedora/gaming package-array suites.

Both suites source the same shape of `*-packages.sh` array and check the same
`setup-*.sh` wrapper contract, so the checks live here once. Importing suites
subclass `PackageArrayChecks` and set `SETUPS`. Import the module (not the
class) so unittest discovery does not also collect the empty base class.
"""

from __future__ import annotations

import importlib.machinery
import importlib.util
import re
import subprocess
import sys
import types
import unittest
from pathlib import Path
from typing import ClassVar

# Shipped by the Sericea base image, so deliberately absent from every array:
# listing one makes `rpm-ostree install` fail with "already provided by" and
# layer nothing at all. See the header of gaming/os/steam-packages.sh.
BASE_IMAGE_PACKAGES = {"gamemode", "7zip"}


def install_command(wrapper: Path, marker: str = "rpm-ostree install") -> str:
    """The actual install line, with comments stripped.

    Matching against the whole file would let an explanatory comment satisfy
    the assertion while the real command differs.
    """
    lines = [
        line
        for line in wrapper.read_text().splitlines()
        if not line.lstrip().startswith("#") and marker in line
    ]
    assert len(lines) == 1, f"{wrapper.name}: expected 1 install line, got {lines}"
    return lines[0]


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


def load_script(name: str, path: Path) -> types.ModuleType:
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


class PackageArrayChecks(unittest.TestCase):
    """Checks every package array + wrapper pair must satisfy.

    SETUPS entries are (package array script, wrapper script, install marker).
    """

    SETUPS: ClassVar[list[tuple[Path, Path, str]]] = []

    def test_arrays_are_non_empty_and_unique(self) -> None:
        for array_script in dict.fromkeys(setup[0] for setup in self.SETUPS):
            with self.subTest(script=array_script.name):
                packages = read_array(array_script)
                self.assertTrue(packages, f"{array_script.name} exports no packages")
                self.assertCountEqual(
                    packages,
                    set(packages),
                    f"{array_script.name} lists a package twice",
                )

    def test_arrays_omit_base_image_packages(self) -> None:
        """A base-image package in the array makes rpm-ostree layer nothing."""
        for array_script in dict.fromkeys(setup[0] for setup in self.SETUPS):
            with self.subTest(script=array_script.name):
                listed = set(read_array(array_script)) & BASE_IMAGE_PACKAGES
                self.assertFalse(
                    listed,
                    f"{array_script.name} lists base-image package(s) "
                    f"{sorted(listed)}; rpm-ostree will fail with 'already "
                    f"provided by' and layer none of the array. Drop them, or "
                    f"add --allow-inactive to the wrapper.",
                )

    def test_wrappers_install_the_sourced_array(self) -> None:
        for array_script, wrapper, marker in self.SETUPS:
            with self.subTest(script=wrapper.name):
                var = f"{array_script.name.split('-')[0]}_packages"
                self.assertIn(f'"${{{var}[@]}}"', install_command(wrapper, marker))

    def test_scripts_are_syntactically_valid(self) -> None:
        for array_script, wrapper, _ in self.SETUPS:
            for script in (array_script, wrapper):
                with self.subTest(script=script.name):
                    subprocess.run(["bash", "-n", str(script)], check=True)
