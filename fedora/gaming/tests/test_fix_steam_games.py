#!/usr/bin/env python3
"""Focused behavior tests for fix-steam-games."""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).parents[1] / "home/.local/bin/fix-steam-games"
BACKEND = b"test backend 4.1.1"
GOOD_INI = "Fsr4Update=true\nFsr4EnableWatermark=auto\nShortcutKey=0x24\n"


class FixSteamGamesTests(unittest.TestCase):
    def test_fsr4_apply_keeps_missing_keys_unresolved(self) -> None:
        with tempfile.TemporaryDirectory() as raw_tmp:
            root = Path(raw_tmp)
            steam = root / "data/Steam"
            common = steam / "steamapps/common"
            (steam / "steamapps/libraryfolders.vdf").parent.mkdir(parents=True)
            (steam / "steamapps/libraryfolders.vdf").write_text(
                f'"libraryfolders"\n{{\n\t"0"\n\t{{\n\t\t"path" "{steam}"\n\t}}\n}}\n'
            )
            proton = common / "Proton Test/contrib"
            proton.mkdir(parents=True)
            (proton / "amdxcffx64.dll").write_bytes(BACKEND)

            games = {
                "Fixed": "Fsr4Update=auto\nFsr4EnableWatermark=true\nShortcutKey=0x2D\n",
                "Incompatible": "OtherSetting=true\n",
                "Okay": GOOD_INI,
                "Skipped": GOOD_INI,
            }
            for appid, (name, ini_text) in enumerate(games.items(), start=100):
                game = common / name
                game.mkdir(parents=True)
                (game / "OptiScaler.ini").write_text(ini_text)
                (steam / f"steamapps/appmanifest_{appid}.acf").write_text(
                    '"AppState"\n{\n'
                    f'\t"appid" "{appid}"\n'
                    f'\t"name" "{name}"\n'
                    f'\t"installdir" "{name}"\n'
                    "}\n"
                )
                if name != "Skipped":
                    (game / "dxgi.dll").write_bytes(b"proxy")
                    (game / "amd_fidelityfx_upscaler_dx12.dll").write_bytes(b"upscaler")
            (common / "Okay/amdxcffx64.dll").write_bytes(BACKEND)

            env = {
                **os.environ,
                "HOME": str(root / "home"),
                "XDG_CONFIG_HOME": str(root / "config"),
                "XDG_DATA_HOME": str(root / "data"),
            }

            dry_run = subprocess.run(
                [str(SCRIPT), "--only", "fsr4"],
                env=env,
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(dry_run.returncode, 1, dry_run.stdout)
            self.assertIn("  drift    100 Fixed:", dry_run.stdout)
            self.assertIn("  incompatible 101 Incompatible:", dry_run.stdout)
            self.assertIn("  ok       102 Okay:", dry_run.stdout)
            self.assertIn("  skip     103 Skipped:", dry_run.stdout)
            self.assertFalse((common / "Fixed/amdxcffx64.dll").exists())
            incompatible_ini = common / "Incompatible/OptiScaler.ini"
            incompatible_before = incompatible_ini.read_bytes()
            self.assertFalse((common / "Incompatible/amdxcffx64.dll").exists())

            applied = subprocess.run(
                [str(SCRIPT), "--only", "fsr4", "--apply", "--force"],
                env=env,
                check=False,
                capture_output=True,
                text=True,
            )
            self.assertEqual(applied.returncode, 1, applied.stdout)
            self.assertIn("  fixed    100 Fixed:", applied.stdout)
            self.assertIn("  incompatible 101 Incompatible:", applied.stdout)
            self.assertIn("  ok       102 Okay:", applied.stdout)
            self.assertIn("  skip     103 Skipped:", applied.stdout)
            self.assertIn(
                "FSR4: 1 updated, 1 already ok, 1 skipped, 1 incompatible.",
                applied.stdout,
            )
            self.assertEqual((common / "Fixed/OptiScaler.ini").read_text(), GOOD_INI)
            self.assertEqual(incompatible_ini.read_bytes(), incompatible_before)
            self.assertFalse((common / "Incompatible/amdxcffx64.dll").exists())


if __name__ == "__main__":
    unittest.main()
