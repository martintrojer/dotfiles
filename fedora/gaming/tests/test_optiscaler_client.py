#!/usr/bin/env python3
"""Focused safety tests for optiscaler-client updates."""

from __future__ import annotations

import hashlib
import importlib.machinery
import importlib.util
import io
import os
import tarfile
import tempfile
import types
import unittest
from pathlib import Path
from typing import Any, cast
from unittest import mock

SCRIPT = Path(__file__).parents[1] / "home/.local/bin/optiscaler-client"


def load_client() -> types.ModuleType:
    loader = importlib.machinery.SourceFileLoader("optiscaler_client_test", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def write_archive(path: Path, members: dict[str, bytes]) -> None:
    with tarfile.open(path, "w:gz") as archive:
        for name, data in members.items():
            info = tarfile.TarInfo(name)
            info.size = len(data)
            archive.addfile(info, io.BytesIO(data))


class OptiscalerClientTests(unittest.TestCase):
    def setUp(self) -> None:
        self.client = cast(Any, load_client())
        self.tempdir = tempfile.TemporaryDirectory()
        self.root = Path(self.tempdir.name)
        self.install = self.root / "optiscaler-client"
        self.client.INSTALL_DIR = self.install
        self.client.VERSION_FILE = self.install / ".version"

    def tearDown(self) -> None:
        self.tempdir.cleanup()

    def make_old_install(self) -> Path:
        self.install.mkdir()
        binary = self.install / self.client.BINARY_NAME
        binary.write_bytes(b"old executable")
        binary.chmod(0o755)
        self.client.VERSION_FILE.write_text("old\n")
        return binary

    def test_bad_checksum_leaves_old_install_runnable(self) -> None:
        binary = self.make_old_install()
        source = self.root / "download.tar.gz"
        write_archive(source, {self.client.BINARY_NAME: b"new executable"})
        release = {"tag_name": "new"}

        def download(_url: str, dest: Path) -> None:
            dest.write_bytes(source.read_bytes())

        with (
            mock.patch.object(self.client, "_fetch_release", return_value=release),
            mock.patch.object(
                self.client,
                "_pick_asset",
                return_value=(
                    source.name,
                    "https://example.invalid/asset",
                    "sha256:" + "0" * 64,
                ),
            ),
            mock.patch.object(self.client, "_download", side_effect=download),
            self.assertRaises(SystemExit),
        ):
            self.client._install(tag=None, force=True)

        self.assertEqual(binary.read_bytes(), b"old executable")
        self.assertTrue(os.access(binary, os.X_OK))
        self.assertEqual(self.client.VERSION_FILE.read_text(), "old\n")

    def test_unsafe_archive_leaves_old_install_runnable(self) -> None:
        binary = self.make_old_install()
        source = self.root / "unsafe.tar.gz"
        write_archive(source, {"../escaped": b"malicious"})
        digest = hashlib.sha256(source.read_bytes()).hexdigest()
        release = {"tag_name": "new"}

        def download(_url: str, dest: Path) -> None:
            dest.write_bytes(source.read_bytes())

        with (
            mock.patch.object(self.client, "_fetch_release", return_value=release),
            mock.patch.object(
                self.client,
                "_pick_asset",
                return_value=(
                    source.name,
                    "https://example.invalid/asset",
                    f"sha256:{digest}",
                ),
            ),
            mock.patch.object(self.client, "_download", side_effect=download),
            self.assertRaises(tarfile.FilterError),
        ):
            self.client._install(tag=None, force=True)

        self.assertEqual(binary.read_bytes(), b"old executable")
        self.assertTrue(os.access(binary, os.X_OK))
        self.assertEqual(self.client.VERSION_FILE.read_text(), "old\n")
        self.assertFalse((self.root / "escaped").exists())

    def test_publication_failure_rolls_back_old_install(self) -> None:
        binary = self.make_old_install()
        staged = self.root / "staged"
        staged.mkdir()
        (staged / self.client.BINARY_NAME).write_bytes(b"new executable")
        real_replace = os.replace

        def fail_publish(source: Path, destination: Path) -> None:
            if Path(source) == staged and Path(destination) == self.install:
                raise OSError("simulated publication failure")
            real_replace(source, destination)

        with (
            mock.patch.object(self.client.os, "replace", side_effect=fail_publish),
            self.assertRaisesRegex(OSError, "simulated publication failure"),
        ):
            self.client._publish(staged)

        self.assertEqual(binary.read_bytes(), b"old executable")
        self.assertTrue(os.access(binary, os.X_OK))
        self.assertEqual(self.client.VERSION_FILE.read_text(), "old\n")

    def test_safe_extract_rejects_traversal_and_links(self) -> None:
        cases = (("../escaped", False), ("link", True))
        for name, is_link in cases:
            with self.subTest(name=name):
                archive_path = self.root / f"{name.replace('/', '_')}.tar"
                with tarfile.open(archive_path, "w") as archive:
                    info = tarfile.TarInfo(name)
                    if is_link:
                        info.type = tarfile.SYMTYPE
                        info.linkname = "target"
                        archive.addfile(info)
                    else:
                        info.size = 1
                        archive.addfile(info, io.BytesIO(b"x"))
                destination = self.root / f"extract-{is_link}"
                destination.mkdir()
                with (
                    tarfile.open(archive_path) as archive,
                    self.assertRaises(tarfile.FilterError),
                ):
                    self.client._safe_extract(archive, destination)

        self.assertFalse((self.root / "escaped").exists())

    def test_successful_install_verifies_and_publishes_staged_payload(self) -> None:
        self.make_old_install()
        source = self.root / "download.tar.gz"
        write_archive(source, {f"payload/{self.client.BINARY_NAME}": b"new executable"})
        digest = hashlib.sha256(source.read_bytes()).hexdigest()
        release = {"tag_name": "new"}

        def download(_url: str, dest: Path) -> None:
            dest.write_bytes(source.read_bytes())

        with (
            mock.patch.object(self.client, "_fetch_release", return_value=release),
            mock.patch.object(
                self.client,
                "_pick_asset",
                return_value=(
                    source.name,
                    "https://example.invalid/asset",
                    f"sha256:{digest}",
                ),
            ),
            mock.patch.object(self.client, "_download", side_effect=download),
        ):
            self.assertEqual(self.client._install(tag=None, force=True), "new")

        binary = self.install / self.client.BINARY_NAME
        self.assertEqual(binary.read_bytes(), b"new executable")
        self.assertTrue(os.access(binary, os.X_OK))
        self.assertEqual(self.client.VERSION_FILE.read_text(), "new\n")


if __name__ == "__main__":
    unittest.main()
