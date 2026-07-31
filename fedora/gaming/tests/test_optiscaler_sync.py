#!/usr/bin/env python3
"""Behavior and failure-safety tests for optiscaler-sync."""

from __future__ import annotations

import hashlib
import importlib.machinery
import importlib.util
import io
import json
import os
import re
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from types import TracebackType
from typing import Any
from unittest import mock

SCRIPT = Path(__file__).parents[1] / "home/.local/bin/optiscaler-sync"


def load_sync() -> Any:
    loader = importlib.machinery.SourceFileLoader("optiscaler_sync_test", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[loader.name] = module
    loader.exec_module(module)
    return module


class Response(io.BytesIO):
    def __enter__(self) -> Response:
        return self

    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: TracebackType | None,
    ) -> None:
        self.close()


class OptiscalerSyncTests(unittest.TestCase):
    def setUp(self) -> None:
        self.sync = load_sync()
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.steam = self.root / "Steam"
        (self.steam / "steamapps/common").mkdir(parents=True)
        self.config = self.steam / "userdata/1/config/localconfig.vdf"
        self.config.parent.mkdir(parents=True)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def app(self, appid: str = "100", name: str = "Game") -> Any:
        game = self.steam / "steamapps/common" / name
        game.mkdir(parents=True, exist_ok=True)
        existing = self.config.read_text() if self.config.exists() else ""
        entries = re.findall(
            r'\t{5}"(\d+)"\n\t{5}\{\n\t{6}"LaunchOptions"\t\t"([^"]*)"\n\t{5}\}',
            existing,
        )
        entries.append((appid, "%command% --skip"))
        app_text = "\n".join(
            f'\t\t\t\t\t"{entry_id}"\n\t\t\t\t\t{{\n'
            f'\t\t\t\t\t\t"LaunchOptions"\t\t"{options}"\n\t\t\t\t\t}}'
            for entry_id, options in entries
        )
        self.config.write_text(
            '"UserLocalConfigStore"\n{\n\t"Software"\n\t{\n\t\t"Valve"\n\t\t{\n'
            '\t\t\t"Steam"\n\t\t\t{\n\t\t\t\t"apps"\n\t\t\t\t{\n'
            f"{app_text}\n"
            "\t\t\t\t}\n\t\t\t}\n\t\t}\n\t}\n}\n"
        )
        return self.sync.SteamApp(appid, name, game, self.steam)

    def payload(self, directory: Path, version: bytes = b"v1") -> dict[Path, Path]:
        source = self.root / f"payload-{version.decode()}"
        source.mkdir()
        (source / "OptiScaler.dll").write_bytes(version)
        (source / "OptiScaler.ini").write_text(
            "ShortcutKey=old\nFsr4Update=true\nKeep=yes\n"
        )
        (source / "support.dll").write_bytes(version + b" support")
        return self.sync.payload_files(source, "dxgi.dll")

    def target(self, app: Any) -> Any:
        return self.sync.Target(app, app.path, "dxgi.dll")

    def test_detection_uses_one_candidate_override_and_manifest_authority(self) -> None:
        app = self.app()
        no_upscaler, reason = self.sync.detect_target(
            app, {"100": {"target": ".", "proxy": "dxgi.dll"}}
        )
        self.assertIsNone(no_upscaler)
        self.assertEqual(reason, "no recognized upscaler DLL")
        first = app.path / "A"
        second = app.path / "B"
        first.mkdir()
        second.mkdir()
        (first / "nvngx_dlss.dll").write_bytes(b"x")
        (second / "libxess.dll").write_bytes(b"x")
        target, reason = self.sync.detect_target(app, {})
        self.assertIsNone(target)
        self.assertEqual(reason, "multiple candidate directories")

        target, reason = self.sync.detect_target(
            app, {"100": {"target": "B", "proxy": "winmm.dll"}}
        )
        self.assertIsNone(reason)
        self.assertEqual(target.directory, second)
        self.assertEqual(target.proxy, "winmm.dll")

        state = first / self.sync.STATE_DIR
        state.mkdir()
        (state / self.sync.MANIFEST).write_text(
            json.dumps(
                {
                    "schema": 1,
                    "appid": "100",
                    "proxy": "version.dll",
                    "files": {},
                    "backups": {},
                    "backup_hashes": {},
                    "launch_configs": [],
                }
            )
        )
        target, reason = self.sync.detect_target(
            app, {"100": {"target": "B", "proxy": "winmm.dll"}}
        )
        self.assertIsNone(reason)
        self.assertEqual(target.directory, first)
        self.assertEqual(target.proxy, "version.dll")

    def test_release_and_archive_validation_reject_untrusted_inputs(self) -> None:
        digest = "1" * 64
        metadata: dict[str, Any] = {
            "tag_name": "v1",
            "draft": False,
            "prerelease": False,
            "assets": [
                {
                    "name": "OptiScaler.7z",
                    "digest": f"sha256:{digest}",
                    "browser_download_url": "https://github.com/optiscaler/OptiScaler/releases/download/v1/OptiScaler.7z",
                }
            ],
        }
        release = self.sync.fetch_release(
            lambda *_args, **_kwargs: Response(json.dumps(metadata).encode())
        )
        self.assertEqual(release.digest, digest)

        bad_metadata: dict[str, Any] = {
            **metadata,
            "assets": [{**metadata["assets"][0], "digest": None}],
        }
        with self.assertRaisesRegex(ValueError, "digest"):
            self.sync.fetch_release(
                lambda *_args, **_kwargs: Response(json.dumps(bad_metadata).encode())
            )

        archive = self.root / "archive.7z"
        archive.write_bytes(b"archive")
        outside = self.root / "escaped"
        self.assertEqual(
            self.sync.archive_entries(
                "----------\nPath = folder\nFolder = +\n\n"
                "Path = folder/file\nFolder = -\nSize = 1\n\n"
            ),
            ["folder", "folder/file"],
        )

        def fake_runner(
            command: list[str], **_kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            if command[1] == "l":
                return subprocess.CompletedProcess(
                    command,
                    0,
                    "----------\nPath = ../escaped\nFolder = -\nSize = 1\n\n",
                    "",
                )
            outside.write_text("bad")
            return subprocess.CompletedProcess(command, 0, "", "")

        extract = self.root / "extract"
        extract.mkdir()
        with self.assertRaisesRegex(ValueError, "unsafe"):
            self.sync.extract_archive(archive, extract, fake_runner)
        self.assertFalse(outside.exists())
        with self.assertRaisesRegex(ValueError, "link"):
            self.sync.archive_entries(
                "----------\nPath = link\nFolder = -\nSymbolic Link = target\n\n"
            )

    def test_paths_cannot_escape_roots_through_symlinks(self) -> None:
        root = self.root / "root"
        outside = self.root / "outside"
        root.mkdir()
        outside.mkdir()
        (root / "link").symlink_to(outside, target_is_directory=True)
        with self.assertRaisesRegex(ValueError, "escaped root"):
            self.sync.child_path(root, "link/file.dll")
        transaction = self.sync.Transaction()
        try:
            with self.assertRaisesRegex(ValueError, "symlink"):
                transaction.capture(root / "link")
        finally:
            transaction.close()

    def test_steam_config_symlinks_are_rejected(self) -> None:
        outside = self.root / "outside.vdf"
        outside.write_text("outside")
        self.config.symlink_to(outside)
        with self.assertRaisesRegex(ValueError, "cannot be a symlink"):
            self.sync.localconfigs(self.steam)

    def test_fake_extractor_yields_valid_filtered_payload(self) -> None:
        archive = self.root / "archive.7z"
        archive.write_bytes(b"fake archive")
        extract = self.root / "extract"
        extract.mkdir()
        listing = (
            "----------\nPath = OptiScaler.dll\nFolder = -\nSize = 3\n\n"
            "Path = OptiScaler.ini\nFolder = -\nSize = 4\n\n"
            "Path = support.dll\nFolder = -\nSize = 7\n\n"
            "Path = setup_windows.bat\nFolder = -\nSize = 5\n\n"
            "Path = !! README !!.txt\nFolder = -\nSize = 6\n\n"
        )

        def fake_runner(
            command: list[str], **_kwargs: object
        ) -> subprocess.CompletedProcess[str]:
            if command[1] == "l":
                return subprocess.CompletedProcess(command, 0, listing, "")
            (extract / "OptiScaler.dll").write_bytes(b"dll")
            (extract / "OptiScaler.ini").write_text("ini\n")
            (extract / "support.dll").write_bytes(b"support")
            (extract / "setup_windows.bat").write_text("setup")
            (extract / "!! README !!.txt").write_text("readme")
            return subprocess.CompletedProcess(command, 0, "", "")

        root = self.sync.extract_archive(archive, extract, fake_runner)
        payload = self.sync.payload_files(root, "dxgi.dll")
        self.assertEqual(
            set(payload),
            {Path("dxgi.dll"), Path("OptiScaler.ini"), Path("support.dll")},
        )

    def test_install_update_preserves_ini_and_restores_collisions_on_uninstall(
        self,
    ) -> None:
        app = self.app()
        (app.path / "dxgi.dll").write_bytes(b"original proxy")
        target = self.target(app)
        release1 = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        self.assertEqual(
            self.sync.install_game(target, self.payload(app.path, b"v1"), release1), []
        )
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"v1")
        self.assertEqual(
            (app.path / "OptiScaler.ini").read_text(),
            "ShortcutKey=0x24\nFsr4Update=auto\nKeep=yes\n",
        )
        self.assertIn('"optirun %command% --skip"', self.config.read_text())
        self.assertTrue(self.config.with_suffix(".vdf.optiscaler-sync.bak").is_file())
        manifest = self.sync.managed_target(app).manifest
        self.assertEqual(
            manifest["launch_configs"], ["userdata/1/config/localconfig.vdf"]
        )
        later_config = self.steam / "userdata/2/config/localconfig.vdf"
        later_config.parent.mkdir(parents=True)
        later_config.write_text(
            self.config.read_text().replace("optirun %command%", "%command%")
        )

        (app.path / "OptiScaler.ini").write_text(
            "ShortcutKey=user\nFsr4Update=true\nKeep=custom\n"
        )
        managed = self.sync.managed_target(app)
        assert managed is not None
        release2 = self.sync.Release("v2", "https://example.invalid", "0" * 64)
        self.sync.install_game(managed, self.payload(app.path, b"v2"), release2)
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"v2")
        self.assertNotIn("optirun %command%", later_config.read_text())
        self.assertEqual(
            (app.path / "OptiScaler.ini").read_text(),
            "ShortcutKey=0x24\nFsr4Update=auto\nKeep=custom\n",
        )

        managed = self.sync.managed_target(app)
        assert managed is not None
        self.sync.uninstall_game(managed, force=False)
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"original proxy")
        self.assertFalse((app.path / "support.dll").exists())
        self.assertFalse((app.path / "OptiScaler.ini").exists())
        self.assertFalse((app.path / self.sync.STATE_DIR).exists())
        self.assertIn('"%command% --skip"', self.config.read_text())
        self.assertNotIn('"optirun %command% --skip"', self.config.read_text())

    def test_update_restores_obsolete_collision_backup(self) -> None:
        app = self.app()
        (app.path / "support.dll").write_bytes(b"original support")
        target = self.target(app)
        release1 = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        payload = self.payload(app.path)
        self.sync.install_game(target, payload, release1)
        payload.pop(Path("support.dll"))
        managed = self.sync.managed_target(app)
        assert managed is not None
        release2 = self.sync.Release("v2", "https://example.invalid", "0" * 64)
        self.sync.install_game(managed, payload, release2)
        self.assertEqual((app.path / "support.dll").read_bytes(), b"original support")
        manifest = self.sync.managed_target(app).manifest
        self.assertNotIn("support.dll", manifest["files"])
        self.assertNotIn("support.dll", manifest["backups"])

    def test_modified_managed_file_is_safe_and_force_uninstall_restores_backup(
        self,
    ) -> None:
        app = self.app()
        (app.path / "dxgi.dll").write_bytes(b"original")
        target = self.target(app)
        release = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        self.sync.install_game(target, self.payload(app.path), release)
        (app.path / "dxgi.dll").write_bytes(b"external")
        managed = self.sync.managed_target(app)
        assert managed is not None

        with self.assertRaisesRegex(ValueError, "modified managed files"):
            self.sync.uninstall_game(managed, force=False)
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"external")
        self.assertTrue((app.path / self.sync.STATE_DIR / self.sync.MANIFEST).is_file())

        (app.path / "support.dll").unlink()
        with self.assertRaisesRegex(ValueError, "support.dll"):
            self.sync.uninstall_game(managed, force=False)

        reports = self.sync.uninstall_game(managed, force=True)
        self.assertIn("dxgi.dll", reports[0])
        self.assertIn("support.dll", reports[0])
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"original")

    def test_corrupt_collision_backup_aborts_uninstall_without_data_loss(self) -> None:
        app = self.app()
        (app.path / "dxgi.dll").write_bytes(b"original")
        release = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        self.sync.install_game(self.target(app), self.payload(app.path), release)
        managed = self.sync.managed_target(app)
        assert managed is not None
        backup = (
            app.path / self.sync.STATE_DIR / managed.manifest["backups"]["dxgi.dll"]
        )
        backup.write_bytes(b"corrupt")

        with self.assertRaisesRegex(ValueError, "corrupt backup"):
            self.sync.uninstall_game(managed, force=False)
        with self.assertRaisesRegex(ValueError, "corrupt backup"):
            self.sync.uninstall_game(managed, force=True)
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"v1")
        self.assertTrue((app.path / self.sync.STATE_DIR).is_dir())
        self.assertIn('"optirun %command% --skip"', self.config.read_text())

    def test_install_failure_rolls_back_game_and_launch_options(self) -> None:
        app = self.app()
        original_config = self.config.read_bytes()
        target = self.target(app)
        release = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        real_atomic = self.sync.atomic_write

        def fail_manifest(path: Path, data: str) -> None:
            if path.name == self.sync.MANIFEST:
                raise OSError("simulated manifest failure")
            real_atomic(path, data)

        with (
            mock.patch.object(self.sync, "atomic_write", side_effect=fail_manifest),
            self.assertRaisesRegex(OSError, "simulated manifest failure"),
        ):
            self.sync.install_game(target, self.payload(app.path), release)
        self.assertFalse((app.path / "dxgi.dll").exists())
        self.assertFalse((app.path / "OptiScaler.ini").exists())
        self.assertEqual(self.config.read_bytes(), original_config)
        self.assertFalse(self.config.with_suffix(".vdf.optiscaler-sync.bak").exists())

        (app.path / "dxgi.dll").write_bytes(b"original")
        with (
            mock.patch.object(
                self.sync, "update_launch_option", side_effect=OSError("VDF failure")
            ),
            self.assertRaisesRegex(OSError, "VDF failure"),
        ):
            self.sync.install_game(target, self.payload(app.path, b"v2"), release)
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"original")
        self.assertFalse((app.path / self.sync.STATE_DIR).exists())

    def test_update_failure_restores_manifest_and_backup_metadata(self) -> None:
        app = self.app()
        (app.path / "dxgi.dll").write_bytes(b"original")
        release = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        self.sync.install_game(self.target(app), self.payload(app.path), release)
        managed = self.sync.managed_target(app)
        assert managed is not None
        manifest_path = app.path / self.sync.STATE_DIR / self.sync.MANIFEST
        manifest_before = manifest_path.read_bytes()
        backup_before = (
            app.path / self.sync.STATE_DIR / managed.manifest["backups"]["dxgi.dll"]
        ).read_bytes()
        real_atomic = self.sync.atomic_write

        def fail_manifest(path: Path, data: str) -> None:
            if path == manifest_path:
                raise OSError("simulated update manifest failure")
            real_atomic(path, data)

        with (
            mock.patch.object(self.sync, "atomic_write", side_effect=fail_manifest),
            self.assertRaisesRegex(OSError, "update manifest failure"),
        ):
            self.sync.install_game(
                managed,
                self.payload(app.path, b"v2"),
                self.sync.Release("v2", "https://example.invalid", "0" * 64),
            )
        self.assertEqual(manifest_path.read_bytes(), manifest_before)
        self.assertEqual(
            (
                app.path / self.sync.STATE_DIR / managed.manifest["backups"]["dxgi.dll"]
            ).read_bytes(),
            backup_before,
        )
        self.assertEqual((app.path / "dxgi.dll").read_bytes(), b"v1")

    def test_bulk_continues_after_game_failure(self) -> None:
        first = self.target(self.app("100", "One"))
        second = self.target(self.app("200", "Two"))
        visited: list[str] = []

        def operation(target: Any) -> list[str]:
            visited.append(target.app.appid)
            if target.app.appid == "100":
                raise OSError("broken")
            (target.directory / "done").write_text("yes")
            return []

        with mock.patch("sys.stderr", new=io.StringIO()):
            self.assertEqual(self.sync.run_bulk([first, second], operation), 1)
        self.assertEqual(visited, ["100", "200"])
        self.assertEqual((second.directory / "done").read_text(), "yes")

    def test_override_file_path_and_talos_skip_are_stable(self) -> None:
        self.assertEqual(
            self.sync.OVERRIDES_FILE,
            Path(__file__).parents[1] / "data/optiscaler-overrides.json",
        )
        overrides = self.sync.load_overrides()
        self.assertEqual(overrides["2677660"]["target"], ".")
        talos = self.app("835960", "Talos")
        (talos.path / "nvngx_dlss.dll").write_bytes(b"x")
        target, reason = self.sync.detect_target(talos, overrides)
        self.assertIsNone(target)
        self.assertEqual(reason, "tracked skip")

    def test_vdf_edit_is_scoped_and_reports_missing_app(self) -> None:
        text = self.config.read_text() if self.config.exists() else ""
        app = self.app("100", "One")
        self.app("200", "Two")
        before = self.config.read_text()
        changed, did_change, found, removed = self.sync.edit_launch(before, app.appid)
        self.assertTrue(did_change)
        self.assertTrue(found)
        self.assertFalse(removed)
        self.assertIn('"optirun %command% --skip"', changed)
        self.assertEqual(changed.count('"%command% --skip"'), 1)
        self.assertEqual(
            self.sync.desired_launch("env FOO=1 optirun %command% --skip"),
            "env FOO=1 optirun %command% --skip",
        )
        quoted = before.replace(
            '"%command% --skip"', '"env FOO=\\"x y\\" %command% --skip"', 1
        )
        quoted_changed, did_change, found, removed = self.sync.edit_launch(
            quoted, app.appid
        )
        self.assertTrue(did_change)
        self.assertTrue(found)
        self.assertFalse(removed)
        self.assertIn('"env FOO=\\"x y\\" optirun %command% --skip"', quoted_changed)
        unchanged, did_change, found, removed = self.sync.edit_launch(before, "999")
        self.assertEqual(unchanged, before)
        self.assertFalse(did_change)
        self.assertFalse(found)
        self.assertFalse(removed)
        self.assertNotEqual(text, before)

    def test_ini_patch_updates_duplicate_keys(self) -> None:
        patched = self.sync.patch_ini(
            "ShortcutKey=old\nshortcutkey=older\nFsr4Update=true\nFSR4UPDATE=false\n"
        )
        self.assertEqual(patched.lower().count("shortcutkey=0x24"), 2)
        self.assertEqual(patched.lower().count("fsr4update=auto"), 2)

    def test_uninstall_only_removes_launch_options_changed_by_install(self) -> None:
        app = self.app()
        second = self.steam / "userdata/2/config/localconfig.vdf"
        second.parent.mkdir(parents=True)
        second.write_text(self.config.read_text())
        release = self.sync.Release("v1", "https://example.invalid", "0" * 64)
        self.sync.install_game(self.target(app), self.payload(app.path), release)
        self.assertIn("optirun %command%", second.read_text())
        self.config.write_text(
            self.config.read_text().replace("optirun %command%", "%command%")
        )

        managed = self.sync.managed_target(app)
        assert managed is not None
        with self.assertRaisesRegex(ValueError, "externally modified"):
            self.sync.install_game(
                managed,
                self.payload(app.path, b"v2"),
                self.sync.Release("v2", "https://example.invalid", "0" * 64),
            )
        with self.assertRaisesRegex(ValueError, "externally modified"):
            self.sync.uninstall_game(managed, force=False)
        self.config.write_text(
            self.config.read_text().replace(
                "%command% --skip", "optirun %command% --skip"
            )
        )
        self.sync.uninstall_game(managed, force=False)
        self.assertIn('"%command% --skip"', self.config.read_text())
        self.assertNotIn("optirun %command%", second.read_text())

    def test_main_dry_run_is_host_safe_and_apply_refuses_running_steam(self) -> None:
        home = self.root / "home"
        data = self.root / "data"
        config = self.root / "config"
        result = subprocess.run(
            [str(SCRIPT)],
            env={
                **os.environ,
                "HOME": str(home),
                "XDG_DATA_HOME": str(data),
                "XDG_CONFIG_HOME": str(config),
            },
            check=False,
            capture_output=True,
            text=True,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("dry run: 0", result.stdout)
        self.assertFalse(home.exists())
        self.assertFalse(data.exists())
        self.assertFalse(config.exists())

        with (
            mock.patch.object(self.sync, "steam_running", return_value=True),
            mock.patch.object(
                self.sync, "steam_roots", side_effect=AssertionError("scanned games")
            ),
            mock.patch("sys.stderr", new=io.StringIO()) as stderr,
        ):
            self.assertEqual(self.sync.main(["--apply"]), 2)
        self.assertIn("quit Steam", stderr.getvalue())

        with (
            mock.patch.object(
                self.sync, "load_overrides", side_effect=ValueError("corrupt overrides")
            ),
            mock.patch("sys.stderr", new=io.StringIO()) as stderr,
        ):
            self.assertEqual(self.sync.main([]), 1)
        self.assertIn("discovery failed: corrupt overrides", stderr.getvalue())
        with (
            mock.patch.object(
                self.sync,
                "load_overrides",
                side_effect=AssertionError("uninstall read overrides"),
            ),
            mock.patch.object(self.sync, "steam_apps", return_value={}),
        ):
            self.assertEqual(self.sync.main(["uninstall"]), 0)

    def test_download_checksum_failure_does_not_publish_data(self) -> None:
        destination = self.root / "download.7z"
        release = self.sync.Release(
            "v1",
            "https://github.com/optiscaler/OptiScaler/releases/download/v1/a.7z",
            hashlib.sha256(b"wanted").hexdigest(),
        )
        with self.assertRaisesRegex(ValueError, "does not match"):
            self.sync.download_release(
                release, destination, lambda *_args, **_kwargs: Response(b"wrong")
            )
        self.assertEqual(destination.read_bytes(), b"wrong")


if __name__ == "__main__":
    unittest.main()
