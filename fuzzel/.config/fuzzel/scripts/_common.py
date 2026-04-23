#!/usr/bin/env python3
"""Shared helpers for fuzzel-based scripts."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import Iterable, Mapping, Sequence


class ScriptError(RuntimeError):
    """Raised for expected user-facing script errors."""


def command_exists(name: str) -> bool:
    return shutil.which(name) is not None


def notify(title: str, message: str) -> None:
    if command_exists("notify-send"):
        subprocess.run(["notify-send", title, message], check=False)


def run(
    cmd: Sequence[str],
    *,
    input_text: str | None = None,
    check: bool = True,
    env: Mapping[str, str] | None = None,
    capture_output: bool = True,
) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        list(cmd),
        input=input_text,
        capture_output=capture_output,
        text=True,
        check=check,
        env=dict(env) if env is not None else None,
    )


def fuzzel_dmenu(
    *,
    prompt: str,
    width: int,
    lines: int | None = None,
    cache: str | None = None,
    options: Iterable[str] | None = None,
    input_text: str | None = None,
) -> str:
    cmd = ["fuzzel", "--dmenu", "--prompt", prompt, "--width", str(width)]
    if lines is not None:
        cmd.extend(["--lines", str(lines)])
    if cache:
        cmd.extend(["--cache", cache])

    if options is not None:
        payload = "\n".join(options)
    else:
        payload = input_text or ""

    result = run(cmd, input_text=payload, check=False)
    if result.returncode != 0:
        return ""
    return result.stdout.strip()


def require_commands(commands: Sequence[str]) -> None:
    missing = [name for name in commands if not command_exists(name)]
    if missing:
        raise ScriptError(f"Missing required command(s): {', '.join(missing)}")


def list_sway_windows(*, mru: bool = False) -> list[dict]:
    """Return a flat list of sway windows extracted from ``swaymsg -t get_tree``.

    Each entry has ``id`` (sway con id), ``app_id`` (Wayland app_id, falling
    back to X11 ``window_properties.class``), ``title`` (window name), and
    ``focused`` (whether the window currently has keyboard focus).

    By default the order matches a depth-first walk of the tree (stable, but
    arbitrary). With ``mru=True`` windows are returned in most-recently-used
    order using each container's ``focus`` array, with the currently focused
    window pushed to the **end** so an Alt-Tab style picker can pre-select the
    most recent *other* window for one-keystroke toggling.

    Returns an empty list if swaymsg fails or the tree cannot be parsed; let
    callers decide how to surface that to the user (notify, ScriptError, etc.).
    """
    result = run(["swaymsg", "-t", "get_tree"], check=False)
    if result.returncode != 0:
        return []
    try:
        tree = json.loads(result.stdout)
    except json.JSONDecodeError:
        return []

    windows: list[dict] = []

    def visit_dfs(node: dict) -> None:
        if _is_window(node):
            windows.append(_window_dict(node))
        for child in node.get("nodes", []) + node.get("floating_nodes", []):
            if isinstance(child, dict):
                visit_dfs(child)

    def collect_workspace_cons(ws: dict) -> list[dict]:
        """Flatten a workspace into a list of windows in its focus order."""
        cons: list[dict] = []

        def visit(node: dict) -> None:
            if _is_window(node):
                cons.append(_window_dict(node))
                return
            children = {
                child["id"]: child
                for child in node.get("nodes", []) + node.get("floating_nodes", [])
                if isinstance(child, dict) and "id" in child
            }
            focus_order = [cid for cid in node.get("focus") or [] if cid in children]
            seen = set(focus_order)
            ordered = focus_order + [cid for cid in children if cid not in seen]
            for cid in ordered:
                visit(children[cid])

        visit(ws)
        return cons

    def collect_mru(root: dict) -> list[dict]:
        """Round-robin workspaces in output-MRU order.

        Sway tracks focus history per-container, not globally, so a naive
        depth-first walk would emit *all* windows from workspace A before
        any from workspace B — even if B was the last place you looked.
        Round-robining (workspace0.focus[0], ws1.focus[0], ws2.focus[0],
        then workspace0.focus[1], ws1.focus[1], ...) gives a global MRU
        that matches user expectation: the previously focused window
        from the previously focused workspace ends up on top.
        """
        # Workspaces in output-MRU order across all outputs.
        workspaces: list[list[dict]] = []
        for output in root.get("nodes", []):
            if not isinstance(output, dict):
                continue
            ws_by_id = {
                ws["id"]: ws
                for ws in output.get("nodes", [])
                if isinstance(ws, dict) and "id" in ws
            }
            focus_order = [wid for wid in output.get("focus") or [] if wid in ws_by_id]
            ordered_ws = focus_order + [
                wid for wid in ws_by_id if wid not in set(focus_order)
            ]
            for wid in ordered_ws:
                ws = ws_by_id[wid]
                if ws.get("name", "").startswith("__"):
                    continue  # skip i3-internal scratch workspaces
                cons = collect_workspace_cons(ws)
                if cons:
                    workspaces.append(cons)

        # Round-robin across workspaces.
        merged: list[dict] = []
        max_len = max((len(ws) for ws in workspaces), default=0)
        for rank in range(max_len):
            for ws in workspaces:
                if rank < len(ws):
                    merged.append(ws[rank])
        return merged

    if isinstance(tree, dict):
        if mru:
            windows = collect_mru(tree)
            # Push the currently focused window to the end so Enter on the
            # picker's first row toggles to the previous window.
            focused_idx = next(
                (i for i, w in enumerate(windows) if w.get("focused")), -1
            )
            if focused_idx >= 0:
                windows.append(windows.pop(focused_idx))
        else:
            visit_dfs(tree)
    return windows


def _is_window(node: dict) -> bool:
    return node.get("type") in {"con", "floating_con"} and node.get("pid") is not None


def _window_dict(node: dict) -> dict:
    props = node.get("window_properties") or {}
    return {
        "id": node.get("id"),
        "app_id": node.get("app_id") or props.get("class") or "",
        "title": node.get("name") or "",
        "focused": bool(node.get("focused")),
    }


def picker_cache_path(name: str) -> str:
    cache_home = Path(os.environ.get("XDG_CACHE_HOME", str(Path.home() / ".cache")))
    fuzzel_cache = cache_home / "fuzzel"
    if fuzzel_cache.exists() and not fuzzel_cache.is_dir():
        # Some setups use ~/.cache/fuzzel as a file; keep picker caches separate.
        cache_dir = cache_home / "fuzzel-pickers"
    else:
        cache_dir = fuzzel_cache / "pickers"
    try:
        cache_dir.mkdir(parents=True, exist_ok=True)
        return str(cache_dir / f"{name}.cache")
    except OSError:
        # Fallback for restricted environments.
        tmp_dir = Path(tempfile.gettempdir()) / "fuzzel-pickers"
        try:
            tmp_dir.mkdir(parents=True, exist_ok=True)
            return str(tmp_dir / f"{name}.cache")
        except OSError:
            # Last resort: disable cache for this run.
            return "/dev/null"
