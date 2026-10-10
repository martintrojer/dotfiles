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
- Busy lock: exits with code `100` and prints ` [name] lock active at [project-root]`

Examples:
- `solo codex`
- `solo -n review claude`
- `solo --force -n planner opencode`

`bubba-lfs-curl-push` uploads Git LFS objects to a Bubba/Forgejo remote through `curl`, then optionally pushes Git refs with `GIT_LFS_SKIP_PUSH=1`. It is a workaround for repos where SSH Git access and `git-lfs-authenticate` work, but the `git-lfs` client's Go HTTP stack cannot connect to Bubba.

Examples:
- `bubba-lfs-curl-push --dry-run`
- `bubba-lfs-curl-push bubba main --push`
- `bubba-lfs-curl-push --ssh-host bubba --ssh-port 3022 --repo-path owner/repo.git --push`

`forgejo-sync` copies every GitHub repo you own (public, private, archived) to Forgejo on Bubba and keeps the copies up to date. It skips forks that have no commits of yours. A repo missing on Forgejo is migrated with the same visibility. A Forgejo pull mirror gets a mirror-sync. A normal repo gets its branches and tags fast-forwarded from GitHub. The script never deletes anything. A ref that moved ahead on Forgejo prints as `DIVERGED`, and the exit code is 1. `--force NAME` overwrites those refs in `NAME` with GitHub's (`FORCED`), using `--force-with-lease` so a ref that changes during the run is left alone. A push the server refuses (hook, protected branch) prints as `REJECTED` with the reason. A fork whose GitHub check fails prints as `ERROR`, never as a silent skip. The GitHub token goes to Forgejo only when migrating a private repo. It does a dry run unless you pass `--apply`. It needs `gh auth login` and a Forgejo token (scopes `write:repository`, `read:user`) in `FORGEJO_TOKEN` or `~/.config/forgejo-sync/token`. Repos listed in `~/.config/forgejo-sync/skip` are left alone.

Examples:
- `forgejo-sync` (dry run)
- `forgejo-sync --apply`
- `forgejo-sync --only rpi --only dotfiles -v`
- `forgejo-sync --apply --force dotfiles --force notes`
