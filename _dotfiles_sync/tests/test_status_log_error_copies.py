#!/usr/bin/env python3
"""Assert the tmux/waybar failure-policy copies stay in sync.

Run: python3 -m unittest discover -s _dotfiles_sync/tests -p 'test_*.py'

`tmux/.config/tmux/scripts/_status_common.py` and
`waybar/.config/waybar/scripts/_state.py` deliberately carry duplicate
copies of `EXPECTED_ERRORS`, `DEFAULT_THROTTLE_SECONDS` and `log_error`.
That duplication is intentional and stays: both files rely on
`sys.path[0]` colocation with the executables that import them, they live
in different packages, and the tmux side has to run on macOS where
`environment.d` does not exist. docs/DECISIONS.md rejects a shared
`pylib/` module for exactly those reasons.

What was missing is enforcement. `_state.py`'s docstring ends with "Keep
the two comparable when either changes." — a note asking a human to
remember. This turns that contract into a check, the same way
test_render_theme.py's AgentGlyphTests pins the generated tmux glyph
chain to `STATE_GLYPH` in `_tmux_common.py`.

This lives in `_dotfiles_sync/tests/` rather than in either package
because the contract spans two packages and belongs to neither; this
directory is the repo-infrastructure suite and already reaches across
packages for the glyph check, so the assertion runs under
`make check-theme`.

Comparison is semantic, not textual: the two modules describe themselves
in their own idiom (status bar vs. Waybar, em-dash vs. `--`), so we
compare the parsed `log_error` with its docstring stripped, plus the
unparsed `EXPECTED_ERRORS` / `DEFAULT_THROTTLE_SECONDS` values. Prose may
diverge; behavior may not.
"""

from __future__ import annotations

import ast
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

TMUX_COMMON = REPO_ROOT / "tmux/.config/tmux/scripts/_status_common.py"
WAYBAR_STATE = REPO_ROOT / "waybar/.config/waybar/scripts/_state.py"

SHARED_CONSTANTS = ("EXPECTED_ERRORS", "DEFAULT_THROTTLE_SECONDS")


def parse(path: Path) -> ast.Module:
    return ast.parse(path.read_text(encoding="utf-8"), filename=str(path))


def top_level_assignment(tree: ast.Module, name: str) -> str:
    """Source text of the module-level `name = ...` value expression."""
    for node in tree.body:
        if isinstance(node, ast.Assign) and any(
            isinstance(t, ast.Name) and t.id == name for t in node.targets
        ):
            return ast.unparse(node.value)
    raise AssertionError(f"no module-level assignment to {name}")


def function_without_docstring(tree: ast.Module, name: str) -> str:
    """Normalized AST dump of `name`, with its docstring removed.

    Signature, decorators and body are all included, so a changed default
    or a reordered statement is drift. The docstring is dropped because
    each module words it for its own bar.
    """
    for node in tree.body:
        if isinstance(node, ast.FunctionDef) and node.name == name:
            func = ast.parse(ast.unparse(node)).body[0]
            assert isinstance(func, ast.FunctionDef)
            if ast.get_docstring(func) is not None:
                func.body = func.body[1:]
            return ast.dump(func)
    raise AssertionError(f"no top-level def {name}")


class FailurePolicyCopyTests(unittest.TestCase):
    """The duplication is intentional; divergence is not."""

    @classmethod
    def setUpClass(cls) -> None:
        cls.tmux = parse(TMUX_COMMON)
        cls.waybar = parse(WAYBAR_STATE)

    def test_shared_constants_match(self) -> None:
        for name in SHARED_CONSTANTS:
            with self.subTest(constant=name):
                self.assertEqual(
                    top_level_assignment(self.tmux, name),
                    top_level_assignment(self.waybar, name),
                    f"{name} drifted between {TMUX_COMMON.name} and "
                    f"{WAYBAR_STATE.name}. The copies are deliberate (see "
                    f"docs/DECISIONS.md) but must stay behaviorally "
                    f"identical: update both.",
                )

    def test_log_error_bodies_match(self) -> None:
        self.assertEqual(
            function_without_docstring(self.tmux, "log_error"),
            function_without_docstring(self.waybar, "log_error"),
            f"log_error drifted between {TMUX_COMMON.name} and "
            f"{WAYBAR_STATE.name}. The copies are deliberate (see "
            f"docs/DECISIONS.md) but must stay behaviorally identical: "
            f"update both. Docstrings are exempt.",
        )

    def test_comparison_is_not_vacuous(self) -> None:
        # Guard the guard: if either helper is renamed or moved, the
        # lookups above must raise rather than silently compare nothing.
        for tree in (self.tmux, self.waybar):
            self.assertIn("OSError", top_level_assignment(tree, "EXPECTED_ERRORS"))
            self.assertIn("log_error", function_without_docstring(tree, "log_error"))


if __name__ == "__main__":
    unittest.main()
