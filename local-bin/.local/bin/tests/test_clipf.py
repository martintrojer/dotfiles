#!/usr/bin/env python3
"""Command-level tests for clipf's Wayland MIME policy."""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

CLIPF = Path(__file__).parents[1] / "clipf"


class ClipfWaylandTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.args = self.root / "args"
        self.payload = self.root / "payload"

        self._tool(
            "file",
            '#!/bin/sh\ncase "$3" in\n  *.jpg) printf image/jpeg;;\n  *.png) printf image/png;;\n  *.pdf) printf application/pdf;;\nesac\n',
        )
        self._tool(
            "wl-copy",
            '#!/bin/sh\nprintf "%s\\n" "$*" > "$CLIPF_ARGS"\ncat > "$CLIPF_PAYLOAD"\n',
        )
        self._tool(
            "magick",
            '#!/bin/sh\nprintf "converted:%s" "$(cat "$1")"\n',
        )
        for name in ("pbcopy", "xclip"):
            self._tool(name, "#!/bin/sh\nexit 99\n")

        self.env = os.environ | {
            "PATH": f"{self.bin}:{os.environ['PATH']}",
            "XDG_SESSION_TYPE": "wayland",
            "CLIPF_ARGS": str(self.args),
            "CLIPF_PAYLOAD": str(self.payload),
        }

    def _tool(self, name: str, body: str) -> None:
        path = self.bin / name
        path.write_text(body)
        path.chmod(0o755)

    def copy_file(
        self, name: str, payload: bytes
    ) -> subprocess.CompletedProcess[bytes]:
        source = self.root / name
        source.write_bytes(payload)
        return subprocess.run([CLIPF, source], env=self.env, capture_output=True)

    def assert_copy(self, mime: str | None, payload: bytes) -> None:
        expected_args = f"-t {mime}\n" if mime else "\n"
        self.assertEqual(self.args.read_text(), expected_args)
        self.assertEqual(self.payload.read_bytes(), payload)

    def test_jpeg_is_converted_and_offered_as_png(self) -> None:
        result = self.copy_file("photo.jpg", b"jpeg bytes")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_copy("image/png", b"converted:jpeg bytes")

    def test_png_keeps_its_original_bytes(self) -> None:
        result = self.copy_file("image.png", b"png bytes")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_copy("image/png", b"png bytes")

    def test_pdf_keeps_its_mime_type_and_bytes(self) -> None:
        result = self.copy_file("document.pdf", b"pdf bytes")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_copy("application/pdf", b"pdf bytes")

    def test_stdin_uses_wl_copy_text_inference(self) -> None:
        result = subprocess.run(
            [CLIPF, "-"], input=b"plain text", env=self.env, capture_output=True
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assert_copy(None, b"plain text")


if __name__ == "__main__":
    unittest.main()
