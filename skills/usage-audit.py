#!/usr/bin/env python3
"""Count skill loads in pi sessions, per skill x model x kind (skills/README.md, zen #6).

Reads the museum store from ~/.config/museum/config.toml (over ssh when [store]
host is set) plus this machine's ~/.pi/agent/sessions, which the store lags.
A load is a `read` tool call on .../.agents/skills/<name>/SKILL.md; reads of the
dotfiles copy are skill edits and are skipped. Each count is sessions, not reads.
Kind: worker (mu workspace folder), delegate (first user message starts
"You are reviewing"), else interactive.
"""

import collections
import json
import re
import subprocess
import sys
import tomllib
from pathlib import Path

LOAD = re.compile(r"/\.agents/skills/([a-z0-9-]+)/SKILL\.md$")


def kind(path: str, first: str) -> str:
    if "mu-workspaces" in path:
        return "worker"
    return "delegate" if first.startswith("You are reviewing") else "interactive"


def scan(root: str) -> None:
    """Runs where the sessions live; prints one 'session skill model kind' row per load."""
    # rg -l first: a single session line can be several MB.
    pattern = r"/\.agents/skills/[a-z0-9-]+/SKILL\.md"
    out = subprocess.run(["rg", "-l0", pattern, root], capture_output=True, text=True)
    for path in filter(None, out.stdout.split("\0")):
        first, seen = None, set()
        with open(path, errors="replace") as lines:
            for line in lines:
                msg = json.loads(line).get("message") or {}
                parts = msg.get("content")
                parts = parts if isinstance(parts, list) else []
                if first is None and msg.get("role") == "user":
                    texts = (
                        p.get("text", "") for p in parts if p.get("type") == "text"
                    )
                    first = next(texts, "")
                for p in parts:
                    if p.get("type") != "toolCall" or p.get("name") != "read":
                        continue
                    arg = str((p.get("arguments") or {}).get("path", ""))
                    m = LOAD.search(arg)
                    if m and "dotfiles/skills/.agents/skills" not in arg:
                        seen.add((m[1], msg.get("model", "?")))
        for skill, model in seen:
            print(Path(path).name, skill, model, kind(path, first or ""))


def run(cmd: list[str], src: str) -> list[str]:
    done = subprocess.run(cmd, input=src, capture_output=True, text=True, check=True)
    return done.stdout.splitlines()


def main() -> None:
    config = Path.home() / ".config/museum/config.toml"
    store = tomllib.loads(config.read_text())["store"]
    src = Path(__file__).read_text()
    here = [sys.executable, "-"]
    host = store.get("host")
    there = ["ssh", "-o", "BatchMode=yes", host, "python3", "-"] if host else here
    rows = run([*there, store["path"]], src)
    rows += run([*here, str(Path.home() / ".pi/agent/sessions")], src)
    # set(): this machine's sessions are also in the store.
    counts = collections.Counter(tuple(r.split()[1:]) for r in set(rows))
    for (skill, model, k), n in sorted(
        counts.items(), key=lambda kv: (kv[0][0], -kv[1], kv[0])
    ):
        print(f"{n:6}  {skill:30} {model:36} {k}")


if __name__ == "__main__":
    scan(sys.argv[1]) if len(sys.argv) > 1 else main()
