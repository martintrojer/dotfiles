# Local Bin

Shared executables that should be available on `$PATH` across macOS and Linux
live here under `.local/bin/`.

`solo` is a Python 3 helper for running a command only once per project root.

- Syntax: `solo [--force] [-n name] <command> [args...]`
- `--force`: remove an unlocked stale state file before running; refuses to clear an active lock
- Root detection: uses `git rev-parse --show-toplevel` first, then walks parent directories looking for `.git`, `.hg`, or `.jj`
- Fallback: if no VCS root is found, it locks against the current directory
- Lock file: `$XDG_STATE_HOME/solo/<project>/<name>.lock` (default base `~/.local/state/solo/`)
- Busy lock: exits with code `100` and prints `❌ [name] lock active at [project-root]`

Examples:
- `solo codex`
- `solo -n review claude`
- `solo --force -n planner opencode`
