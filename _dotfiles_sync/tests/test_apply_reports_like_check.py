#!/usr/bin/env python3
"""--apply reports leftover problems the way --check does, and fails on them.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

--apply used to run only apply steps. A problem with no apply step behind it
(a removed TPM plugin, say) was reported by --check and silently passed by
--apply, which printed "Done." and exited 0. main() now ends every run with
the check pass; these tests pin that --apply surfaces its result.
"""

from __future__ import annotations

import io
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path
from unittest import mock

sys.path.insert(0, str(Path(__file__).resolve().parents[2]))

from _dotfiles_sync import cli
from _dotfiles_sync.model import Action, Args


class ApplyReportsLikeCheckTest(unittest.TestCase):
    def run_main(self, action: Action, *, check_finds_issue: bool) -> tuple[int, str]:
        with tempfile.TemporaryDirectory() as target:
            args = Args(
                action=action,
                force_overwrite=False,
                show_diffs=False,
                verbose=False,
                target=target,
                ignore=set(),
                packages=("tmux",),
                skip_gaming=False,
            )
            patches = (
                mock.patch.object(cli, "parse_args", return_value=args),
                mock.patch.object(cli, "run_apply_group"),
                mock.patch.object(cli, "run_apply_tasks"),
                mock.patch.object(cli, "run_check_group", return_value=False),
                mock.patch.object(
                    cli, "run_check_tasks", return_value=check_finds_issue
                ),
            )
            for patch in patches:
                patch.start()
                self.addCleanup(patch.stop)
            out = io.StringIO()
            with redirect_stdout(out):
                code = cli.main()
        return code, out.getvalue()

    def test_apply_fails_when_a_check_still_finds_an_issue(self) -> None:
        code, out = self.run_main("apply", check_finds_issue=True)
        self.assertEqual(code, 1)
        self.assertIn("issues remain", out)
        self.assertNotIn("Done.", out)

    def test_apply_succeeds_when_checks_are_clean(self) -> None:
        code, out = self.run_main("apply", check_finds_issue=False)
        self.assertEqual(code, 0)
        self.assertIn("Done.", out)

    def test_check_exit_code_matches_apply(self) -> None:
        for finds in (True, False):
            with self.subTest(check_finds_issue=finds):
                check_code, _ = self.run_main("check", check_finds_issue=finds)
                apply_code, _ = self.run_main("apply", check_finds_issue=finds)
                self.assertEqual(check_code, apply_code)


if __name__ == "__main__":
    unittest.main()
