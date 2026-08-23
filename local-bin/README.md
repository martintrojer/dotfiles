# Local Bin

Shared executables that should be available on `$PATH` across macOS and Linux
live here under `.local/bin/`.

`solo` is a Python 3 helper for running a command only once per project root.

- Syntax: `solo [--force] [-n name] <command> [args...]`
- `--force`: remove an abandoned lock file before running, such as after a
  previous `solo` process was killed. The option refuses to clear an active lock
- Root detection: uses `git rev-parse --show-toplevel` first, then walks parent directories looking for `.git`, `.hg`, or `.jj`
- Fallback: if no VCS root is found, it locks against the current directory
- Lock file: `$XDG_STATE_HOME/solo/<project>/<name>.lock` (default base
  `~/.local/state/solo/`). `solo` removes it on normal exit, so only interrupted
  processes leave orphaned locks
- Busy lock: exits with code `100` and prints `❌ [name] lock active at [project-root]`

Examples:
- `solo codex`
- `solo -n review claude`
- `solo --force -n planner opencode`

`bubba-lfs-curl-push` uploads Git LFS objects to a Bubba/Forgejo remote through `curl`, then optionally pushes Git refs with `GIT_LFS_SKIP_PUSH=1`. It is a workaround for repos where SSH Git access and `git-lfs-authenticate` work, but the `git-lfs` client's Go HTTP stack cannot connect to Bubba.

Examples:
- `bubba-lfs-curl-push --dry-run`
- `bubba-lfs-curl-push bubba main --push`
- `bubba-lfs-curl-push --ssh-host bubba --ssh-port 3022 --repo-path owner/repo.git --push`
