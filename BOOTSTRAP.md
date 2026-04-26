# Bootstrap

Two things live here:

1. **Fresh-machine install** — the canonical happy path.
2. **Testing the bootstrap without clobbering your real `$HOME`** — recipes for validating `stow-all.py` changes before pushing or before bootstrapping a real new machine.

The root [`README.md`](./README.md) has a one-liner version of #1; this file is the long version with the testing recipes.

## Fresh-machine install

```bash
git clone https://github.com/martintrojer/dotfiles ~/dotfiles
cd ~/dotfiles
./stow-all.py --apply --install-agents
# Then add the codex notify line to ~/.codex/config.toml (printed by --apply).
```

`--apply` always:

- Stows the dotfile packages that match the current OS and distro.
- Clones the pinned zsh plugins into `~/.local/share/zsh-plugins/`.
- Symlinks `dotfiles/skills/*` into `~/.agents/skills/` (the universal path read by Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw).
- Symlinks `dotfiles/pi/extensions/*.ts` into `~/.pi/agent/extensions/` (Pi auto-discovers these).
- Prunes stale symlinks (skills you removed from the repo).

`--install-agents` adds:

- Claude marketplace registration (one-time): `claude plugin marketplace add martintrojer/dotfiles`.
- Claude plugin install/refresh: `claude plugin install mtrojer@dotfiles`, re-run only when the repo's git HEAD has advanced past the cached snapshot SHA.

Without `--install-agents`, `--apply` prints the exact two `claude` commands you'd need to run by hand.

## Update flow

When any of the agent-side content changes in this repo:

- **Skills and pi extensions:** nothing to do. The `~/.agents/skills/<name>` and `~/.pi/agent/extensions/<name>.ts` symlinks point straight at the repo source; edits propagate live.
- **New / removed skills:** re-run `./stow-all.py --apply` to create new symlinks or prune stale ones.
- **Claude (any change to `agents/`, `commands/`, `hooks/`, or `skills/`):** push to `origin`, then re-run `./stow-all.py --apply --install-agents` on each consumer machine. The plugin install only re-fetches when the repo HEAD has actually advanced past the cached snapshot.

## Testing the bootstrap

Two recipes for dry-running the install path before pushing or before bootstrapping a real new machine.

### Quick: `--target` against a tmpfs path (5 seconds)

```bash
rm -rf /tmp/fresh-home && mkdir /tmp/fresh-home
./stow-all.py --target=/tmp/fresh-home --apply
# Inspect:
ls -la /tmp/fresh-home/
ls /tmp/fresh-home/.agents/skills/
ls /tmp/fresh-home/.local/share/zsh-plugins/
```

Validates everything `--apply` does (stow, zsh plugin clones, skill + pi-extension symlinks). Doesn't exercise `--install-agents` because `claude` / `pi` / `npx` resolve home-relative paths internally and would still touch your real `~/.claude/`, `~/.pi/` etc.

### Full: throwaway container with a fake `$HOME`

When you want to verify the actual fresh-user experience end-to-end — including the interactive zsh, fzf bindings, agent-attention hooks, etc. — spin up a podman container with `HOME` pointed at a tmpfs path inside the container. Toolbox itself bind-mounts your real `$HOME`, so it's not the right tool here; use plain `podman run`:

```bash
LOCAL_REPO="$(pwd)"   # if testing pre-push changes; otherwise omit -v below

podman run --rm -it \
  --userns=keep-id \
  --hostname dotfiles-fresh \
  -e "HOME=/home/test" \
  -w /home/test \
  -v "$LOCAL_REPO:/src/dotfiles:ro" \
  registry.fedoraproject.org/fedora-toolbox:latest \
  bash -lc '
    set -euxo pipefail
    sudo dnf install -y --quiet git stow zsh python3 fzf zoxide eza ripgrep fd-find tmux curl
    cp -a /src/dotfiles ~/dotfiles    # or: git clone https://github.com/martintrojer/dotfiles ~/dotfiles
    cd ~/dotfiles
    ./stow-all.py --apply --install-agents
    exec zsh -l
  '
```

Drop the `-v ...:/src/dotfiles:ro` and replace `cp -a ...` with the `git clone` line to test the real github fresh-bootstrap path. The container is auto-removed on exit (`--rm`).

Notes:

- `claude`, `pi`, and `npx` aren't in the base toolbox image, so `--install-agents` will print SKIP messages for each. That's also a useful test (verifies the SKIP-on-missing-CLI behavior actually fires).
- If you want to test the agent installs too, install them in the container first (`mise install` etc.) before running `--apply`.
- Container is not part of the daily flow — use this only before pushing a stow-all change or before bootstrapping a new physical machine.
