#!/usr/bin/env python3
"""Behavior tests for the package arrays and their rpm-ostree wrappers.

Regression guard: gamemode and 7zip ship in the Sericea base image, so a plain
`rpm-ostree install` rejects them with "already provided by" and installs
nothing -- taking every other package in the array down with it. Listing such a
package is fine, but only when the wrapper passes --allow-inactive.
"""

from __future__ import annotations

import re
import subprocess
import unittest
from pathlib import Path

FEDORA = Path(__file__).parents[2]

# (package array script, wrapper script)
SETUPS = [
    (FEDORA / "os/base-packages.sh", FEDORA / "os/setup-base.sh"),
    (FEDORA / "os/sway-packages.sh", FEDORA / "os/setup-sway.sh"),
    (FEDORA / "gaming/os/steam-packages.sh", FEDORA / "gaming/os/setup-steam.sh"),
]

# Packages known to ship in the Fedora Sway Atomic base image. Installing these
# requires --allow-inactive in the wrapper that consumes the array.
BASE_IMAGE_PACKAGES = {"gamemode", "7zip"}


def install_command(wrapper: Path) -> str:
    """The actual `rpm-ostree install` line, with comments stripped.

    Matching against the whole file would let an explanatory comment mentioning
    --allow-inactive satisfy the assertion while the real command lacks it.
    """
    lines = [
        line
        for line in wrapper.read_text().splitlines()
        if not line.lstrip().startswith("#") and "rpm-ostree install" in line
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

    def test_base_image_packages_require_allow_inactive(self):
        """A base-image package in the array forces --allow-inactive."""
        for array_script, wrapper in SETUPS:
            with self.subTest(script=wrapper.name):
                packages = set(read_array(array_script))
                overlap = packages & BASE_IMAGE_PACKAGES
                command = install_command(wrapper)
                if overlap:
                    self.assertIn(
                        "--allow-inactive",
                        command,
                        f"{wrapper.name} installs base-image package(s) "
                        f"{sorted(overlap)} without --allow-inactive; rpm-ostree "
                        f"will fail with 'already provided by' and install nothing",
                    )
                else:
                    self.assertNotIn(
                        "--allow-inactive",
                        command,
                        f"{wrapper.name} passes --allow-inactive but lists no "
                        f"base-image package; drop the flag so genuine typos "
                        f"still fail loudly",
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


if __name__ == "__main__":
    unittest.main()
