#!/usr/bin/env python3
"""Shared helpers for fuzzel-based scripts."""

from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Callable, Iterable, Mapping, Sequence


class ScriptError(RuntimeError):
    """Raised for expected user-facing script errors."""


def cli_main(name: str, main_fn: Callable[[], int]) -> None:
    """Standard entrypoint wrapper for fuzzel scripts.

    Catches ``ScriptError``, prints ``name: message`` to stderr, exits 1.
    Anything else propagates so unexpected bugs aren't hidden behind a
    user-facing error message.
    """
    try:
        rc = main_fn()
    except ScriptError as exc:
        print(f"{name}: {exc}", file=sys.stderr)
        raise SystemExit(1)
    raise SystemExit(rc)


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
    tree = _get_sway_tree()
    if tree is None:
        return []
    if mru:
        return _focused_last(_collect_mru(tree))
    return _collect_dfs(tree)


def _get_sway_tree() -> dict | None:
    """Run ``swaymsg -t get_tree`` and return the parsed root, or ``None``."""
    result = run(["swaymsg", "-t", "get_tree"], check=False)
    if result.returncode != 0:
        return None
    try:
        tree = json.loads(result.stdout)
    except json.JSONDecodeError:
        return None
    return tree if isinstance(tree, dict) else None


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


def _collect_dfs(node: dict) -> list[dict]:
    """Walk the tree depth-first and return windows in tree order."""
    out: list[dict] = []
    if _is_window(node):
        out.append(_window_dict(node))
    for child in (*node.get("nodes", []), *node.get("floating_nodes", [])):
        if isinstance(child, dict):
            out.extend(_collect_dfs(child))
    return out


def _collect_workspace_cons(ws: dict) -> list[dict]:
    """Flatten a workspace into a list of windows in its container focus order.

    Within the workspace, walk children in the order recorded by each
    container's ``focus`` array (most-recently-focused first), falling back
    to the natural child order for any IDs not listed (e.g. newly created
    windows that haven't been touched yet).
    """
    if _is_window(ws):
        return [_window_dict(ws)]
    children = {
        child["id"]: child
        for child in ws.get("nodes", []) + ws.get("floating_nodes", [])
        if isinstance(child, dict) and "id" in child
    }
    focus_order = [cid for cid in ws.get("focus") or [] if cid in children]
    seen = set(focus_order)
    ordered = focus_order + [cid for cid in children if cid not in seen]
    out: list[dict] = []
    for cid in ordered:
        out.extend(_collect_workspace_cons(children[cid]))
    return out


def _collect_mru(root: dict) -> list[dict]:
    """Round-robin windows across workspaces in output-MRU order.

    Sway tracks focus history per-container, not globally, so a naive
    depth-first walk would emit *all* windows from workspace A before any
    from workspace B — even if B was the last place you looked.
    Round-robining (ws0.focus[0], ws1.focus[0], ws2.focus[0], then
    ws0.focus[1], ws1.focus[1], ...) gives a global MRU that matches user
    expectation: the previously focused window from the previously focused
    workspace ends up on top.
    """
    workspaces = _ordered_workspaces(root)
    merged: list[dict] = []
    max_len = max((len(ws) for ws in workspaces), default=0)
    for rank in range(max_len):
        for ws in workspaces:
            if rank < len(ws):
                merged.append(ws[rank])
    return merged


def _ordered_workspaces(root: dict) -> list[list[dict]]:
    """Return per-workspace window lists in output-MRU order.

    Skips i3-internal scratch workspaces (names starting with ``__``) and
    workspaces with no real windows (no ``pid``).
    """
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
        seen = set(focus_order)
        ordered_ws = focus_order + [wid for wid in ws_by_id if wid not in seen]
        for wid in ordered_ws:
            ws = ws_by_id[wid]
            if ws.get("name", "").startswith("__"):
                continue
            cons = _collect_workspace_cons(ws)
            if cons:
                workspaces.append(cons)
    return workspaces


def _focused_last(windows: list[dict]) -> list[dict]:
    """Return ``windows`` with the currently-focused entry moved to the end.

    No-op if no entry is marked ``focused``. Used by MRU mode so the picker
    pre-selects the most recent *other* window on row 0.
    """
    focused_idx = next((i for i, w in enumerate(windows) if w.get("focused")), -1)
    if focused_idx < 0:
        return windows
    out = list(windows)
    out.append(out.pop(focused_idx))
    return out


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
