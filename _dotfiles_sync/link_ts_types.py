#!/usr/bin/env python3
"""Link the type packages `make check-ts` needs into a repo-root node_modules.

The TypeScript here (pi extensions, opencode plugin) is loaded by globally
installed hosts, so the repo has no package.json and no installed deps. tsc
resolves bare specifiers by walking up from the source file into node_modules
and never looks in the npm global prefix, so build a symlink farm at the repo
root pointing at the already-installed copies.

tsconfig `paths` cannot replace this: under moduleResolution nodenext a mapped
path bypasses the package's `exports` map, so subpath imports like
"@earendil-works/pi-ai/compat" stop resolving. Real symlinks keep exports
working. node_modules is gitignored; nothing here is committed.
"""

from __future__ import annotations

import shutil
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
NODE_MODULES = REPO_ROOT / "node_modules"
PI_PKG = "@earendil-works/pi-coding-agent"
# pi-ai / pi-tui / typebox are pi's own deps, not separate top-level installs.
PI_VENDORED = ("@earendil-works/pi-ai", "@earendil-works/pi-tui", "typebox", "@types")


def npm_global_root() -> Path:
    npm = shutil.which("npm")
    if npm is None:
        sys.exit("link-ts-types: npm not on PATH")
    done = subprocess.run(
        [npm, "root", "-g"], capture_output=True, text=True, check=True
    )
    return Path(done.stdout.strip())


def opencode_sdk_root() -> Path:
    """opencode installs its plugin SDK beside the user's opencode config."""
    for base in (Path.home() / ".config/opencode", Path.home() / ".opencode"):
        candidate = base / "node_modules/@opencode-ai"
        if candidate.is_dir():
            return candidate
    sys.exit(
        "link-ts-types: @opencode-ai not found under ~/.config/opencode or ~/.opencode"
    )


def link(target: Path, name: str) -> None:
    if not target.exists():
        sys.exit(f"link-ts-types: missing {name} at {target}")
    dest = NODE_MODULES / name
    dest.parent.mkdir(parents=True, exist_ok=True)
    if dest.is_symlink():
        dest.unlink()
    elif dest.exists():
        sys.exit(f"link-ts-types: {dest} exists and is not a symlink")
    dest.symlink_to(target)


def main() -> int:
    pi = npm_global_root() / PI_PKG
    if not pi.is_dir():
        sys.exit(
            f"link-ts-types: {PI_PKG} is not installed globally (looked in {pi.parent})"
        )

    link(pi, PI_PKG)
    for name in PI_VENDORED:
        link(pi / "node_modules" / name, name)
    link(opencode_sdk_root(), "@opencode-ai")

    print(
        f"link-ts-types: linked {len(PI_VENDORED) + 2} type packages into {NODE_MODULES}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
