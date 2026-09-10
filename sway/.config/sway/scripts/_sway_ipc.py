"""Package-local swaymsg / get_tree. Imported via sys.path[0] colocation.

Not a cross-package pylib — see docs/DECISIONS.md. Fuzzel's tree walk stays
in fuzzel/_common.py; the walks in preset-width and window-back-and-forth
diverge after this shared fetch.
"""

from __future__ import annotations

import json
import subprocess


def swaymsg(*args: str) -> str:
    return subprocess.check_output(["swaymsg", *args], text=True)


def get_tree():
    """Run ``swaymsg -t get_tree`` and return the parsed root, or ``None``."""
    try:
        raw = swaymsg("-t", "get_tree")
    except (OSError, subprocess.CalledProcessError):
        return None
    try:
        tree = json.loads(raw)
    except json.JSONDecodeError:
        return None
    return tree if isinstance(tree, dict) else None
