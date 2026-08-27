#!/usr/bin/env python3
"""Command-level tests for the cliphist clipboard picker."""

from __future__ import annotations

import os
import subprocess
import tempfile
import unittest
from pathlib import Path

CLIPBOARD = Path(__file__).parents[1] / "clipboard"


class ClipboardPickerTests(unittest.TestCase):
    def setUp(self) -> None:
        tmp = tempfile.TemporaryDirectory()
        self.addCleanup(tmp.cleanup)
        self.root = Path(tmp.name)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.log = self.root / "log"
        self.decode_input = self.root / "decode-input"
        self.payload = self.root / "payload"
        self.selection = "42\t[binary data image/png]"

        self._tool(
            "cliphist",
            f"""#!/bin/sh
printf 'cliphist %s\\n' "$*" >> "$TEST_LOG"
case "$1" in
  list) printf '7\\tfirst item\\n{self.selection}\\n';;
  decode) cat > "$TEST_DECODE_INPUT"; printf AP9iaW5hcnk= | base64 -d;;
esac
""",
        )
        self._tool(
            "fuzzel",
            """#!/bin/sh
cat >/dev/null
[ "${TEST_CANCEL:-0}" = 1 ] && exit 1
printf '%s\n' "$TEST_SELECTION"
""",
        )
        self._tool(
            "wl-copy",
            """#!/bin/sh
printf 'wl-copy %s\n' "$*" >> "$TEST_LOG"
cat > "$TEST_PAYLOAD"
""",
        )

        self.env = os.environ | {
            "PATH": f"{self.bin}:{os.environ['PATH']}",
            "TEST_LOG": str(self.log),
            "TEST_DECODE_INPUT": str(self.decode_input),
            "TEST_PAYLOAD": str(self.payload),
            "TEST_SELECTION": self.selection,
        }

    def _tool(self, name: str, body: str) -> None:
        path = self.bin / name
        path.write_text(body)
        path.chmod(0o755)

    def run_picker(self, **env: str) -> subprocess.CompletedProcess[bytes]:
        return subprocess.run(
            [CLIPBOARD], env=self.env | env, capture_output=True, timeout=5
        )

    def test_selected_row_is_decoded_to_wl_copy_as_binary(self) -> None:
        result = self.run_picker()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            self.log.read_text().splitlines(),
            ["cliphist list", "cliphist decode", "wl-copy "],
        )
        self.assertEqual(self.decode_input.read_text(), self.selection + "\n")
        self.assertEqual(self.payload.read_bytes(), b"\x00\xffbinary")

    def test_cancel_does_not_change_the_clipboard(self) -> None:
        result = self.run_picker(TEST_CANCEL="1")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.log.read_text().splitlines(), ["cliphist list"])
        self.assertFalse(self.decode_input.exists())
        self.assertFalse(self.payload.exists())


if __name__ == "__main__":
    unittest.main()
