#!/usr/bin/env python3
"""Shared helpers for fuzzel-based scripts."""

from __future__ import annotations

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
