# Setup

Everything about getting this repo onto a machine — fresh installs, upgrading machines that are running an older version, day-to-day updates, and recipes for testing changes without clobbering your real `$HOME`.

Pick the section that matches your starting state:

- **Fresh machine** → [Fresh install](#fresh-install).
- **Existing machine on an older version of this repo** (OMZ-based zsh, manually-cloned TPM, per-agent skill copies, etc.) → [Upgrading from an older setup](#upgrading-from-an-older-setup).
- **Already current, just want to pull and re-apply changes** → [Update flow](#update-flow).
- **Hacking on `dotfiles-sync` itself** → [Testing the bootstrap](#testing-the-bootstrap).

## Quick start

```bash
git clone https://github.com/martintrojer/dotfiles ~/dotfiles
cd ~/dotfiles
./dotfiles-sync --apply
# Then run the two manual commands the script prints:
#   - Claude Code plugin install
#   - Codex notify hook (one TOML line in ~/.codex/config.toml)
```

That covers the happy path on a fresh machine. The sections below expand each piece.

## Fresh install

`./dotfiles-sync --apply` always:

- Stows the dotfile packages that match the current OS and distro.
- Clones the pinned zsh plugins into `~/.local/share/zsh-plugins/`.
- Clones TPM (tmux plugin manager) into `~/.tmux/plugins/tpm/` at the pinned ref. Tmux plugins listed in `.tmux.conf` still need a one-time `prefix + I` inside tmux to install — TPM owns that step.
- Symlinks `dotfiles/skills/*` into `~/.agents/skills/` (the universal path read by Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw).
- Symlinks `dotfiles/pi/extensions/*.ts` into `~/.pi/agent/extensions/` (Pi auto-discovers these).
- Prunes stale symlinks when you remove a skill or Pi extension from the repo.

After `--apply` completes it prints two manual follow-ups, both of which `--apply` deliberately does **not** automate:

1. **Claude Code plugin** — Claude wants its own plugin cache (it copies the marketplace content into `~/.claude/plugins/cache/...`); there is no symlink path in. Run once per machine:

   ```bash
   claude plugin marketplace add martintrojer/dotfiles
   claude plugin install mtrojer@dotfiles
   ```

   Re-run just the second command (`claude plugin install mtrojer@dotfiles`) after every push to refresh.

2. **Codex notify hook** — add the single `notify = [...]` line the closing hint prints into `~/.codex/config.toml`. Editing the user's TOML in-place would mean parsing/rewriting their schema; not worth it for one line.

## Upgrading from an older setup

For machines that have been running an older version of this repo (the OMZ-based zsh, the per-agent skill copies, the manually-cloned TPM, etc.). The doc is organized as **detect → action** per item. Run the detection commands; only do the action if the detection finds something. Items are independent — a partial-upgrade machine can run only the relevant sections.

### Step 0: pull the latest, run apply

```bash
cd ~/dotfiles
git pull
./dotfiles-sync --check    # surface stow conflicts before they bite
./dotfiles-sync --apply
# Then re-run the Claude plugin install if you want the latest marketplace
# payload on this machine:
#   claude plugin install mtrojer@dotfiles
# (The universal ~/.agents/skills and ~/.pi/agent/extensions symlinks are
# already live after --apply.)
```

`--apply` is idempotent and handles most of the common cases automatically (stows the new packages, clones zsh-plugins + TPM, symlinks skills + Pi extensions). The cleanup steps below cover the things `--apply` deliberately doesn't touch — vestigial files left by the old layout that aren't actively harmful but waste disk and confuse future-you.

### 1. oh-my-zsh leftovers

The repo dropped OMZ in favor of a slim native `.zshrc` (see [`DECISIONS.md` § oh-my-zsh](./DECISIONS.md)). The new `.zshrc` is already in place after `--apply`, but the old `~/.oh-my-zsh/` directory and any `~/.zshrc.pre-oh-my-zsh` backup file are now orphaned.

Detect:

```bash
ls -d ~/.oh-my-zsh ~/.zshrc.pre-oh-my-zsh 2>/dev/null
du -sh ~/.oh-my-zsh 2>/dev/null
```

Action:

```bash
rm -rf ~/.oh-my-zsh
rm -f ~/.zshrc.pre-oh-my-zsh
# Open a fresh shell to confirm the new .zshrc loads cleanly:
zsh -l -c 'echo "ZSH_VERSION=$ZSH_VERSION; ZSH=${ZSH:-unset}"'
# Expect: ZSH_VERSION=5.x.x; ZSH=unset
```

If `ZSH=` is still set to a path under `.oh-my-zsh`, something else (a sourced file outside this repo, an env var in `~/.zshenv`, or a system-level zsh init) is exporting it; grep your shell init for `oh-my-zsh` and remove.

### 2. Old zsh-plugin location

Older setups cloned `zsh-autosuggestions` and `zsh-syntax-highlighting` into `~/.zsh/plugins/<name>/`. The new path is `~/.local/share/zsh-plugins/<name>/` (what `dotfiles-sync --apply` populates; see `_dotfiles_sync/pins.py` for the pinned plugin definitions and destination path).

Detect:

```bash
ls -la ~/.zsh/plugins/ 2>/dev/null
ls -la ~/.local/share/zsh-plugins/ 2>/dev/null
```

If both exist, only the new one is being sourced by the new `.zshrc`; the old one is dead weight.

Action:

```bash
rm -rf ~/.zsh/plugins
```

### 3. Per-agent skill copies (the old fan-out)

Older versions of the repo bootstrap (~1370 lines before the `_dotfiles_sync/` split, see commit `b29b3003`) copied each skill into per-agent locations: `~/.codex/skills/`, `~/.agents/skills/`, the Claude plugin bundle. The new model is a single set of symlinks at `~/.agents/skills/` that the supported non-Claude agents read natively. Old per-agent copies are now stale — they won't get updates from the repo, and they may shadow the canonical symlinks.

Detect:

```bash
# These should be empty / nonexistent on a current machine:
ls -la ~/.codex/skills 2>/dev/null
ls -la ~/.cursor/skills 2>/dev/null
ls -la ~/.amp/skills 2>/dev/null
ls -la ~/.cline/skills 2>/dev/null

# This should exist as symlinks, one per repo skill:
ls -la ~/.agents/skills/
```

If `~/.agents/skills/` entries are *files or directories* rather than *symlinks pointing into your dotfiles repo*, you have an old copy-based layout.

Action:

```bash
# Remove the old per-agent skill copies. Each agent will fall back to
# the universal ~/.agents/skills/ path that --apply populated.
rm -rf ~/.codex/skills ~/.cursor/skills ~/.amp/skills ~/.cline/skills

# If ~/.agents/skills/ contains real directories instead of symlinks, blow it
# away and let --apply rebuild as symlinks:
rm -rf ~/.agents/skills
cd ~/dotfiles && ./dotfiles-sync --apply

# Verify symlinks now point into the repo:
ls -la ~/.agents/skills/ | head
# Expect each line to be: lrwxrwxrwx ... <name> -> /path/to/dotfiles/skills/<name>
```

### 4. Old Claude plugin install (local bundle)

The Claude plugin used to be installed as a local bundle (filesystem path). The new model is the GitHub marketplace (`claude plugin marketplace add martintrojer/dotfiles`).

Detect:

```bash
claude plugin list 2>/dev/null
# Look for any "mtrojer" or "dotfiles" plugin installed from a local path
# rather than from the GitHub marketplace.
```

Action:

```bash
# Remove the old local install (replace <name> with whatever `claude plugin list`
# shows for the local install):
claude plugin uninstall <name>

# Install fresh from the GitHub marketplace:
claude plugin marketplace add martintrojer/dotfiles
claude plugin install mtrojer@dotfiles
```

If you already ran the marketplace add as part of step 0's post-apply hint, only the *removal* of the old local install is needed here.

### 5. Manual TPM clone

Older instructions in `tmux/README.md` told you to `git clone .../tpm` manually. `dotfiles-sync --apply` now handles this and pins TPM to a known ref (currently `v3.1.0`).

Detect:

```bash
cd ~/.tmux/plugins/tpm 2>/dev/null && git rev-parse HEAD && git describe --tags --exact-match HEAD 2>&1
```

If `git describe --tags --exact-match HEAD` reports `fatal: no tag exactly matches ...`, your TPM is at a non-pinned commit (probably master HEAD from when you originally cloned).

Action:

```bash
cd ~/dotfiles && ./dotfiles-sync --apply
# This will print "PINNED: tpm @ v3.1.0 (...)". Then verify:
cd ~/.tmux/plugins/tpm && git describe --tags --exact-match HEAD
# Expect: v3.1.0
```

The `~/.tmux/plugins/<other-plugins>/` directories (the actual @plugin entries listed in `.tmux.conf`) are still owned by TPM and updated via `prefix + U` inside tmux. Don't touch those.

### 6. Codex notify hook

`~/.codex/config.toml` should contain the top-level `notify = [...]` line that `./dotfiles-sync --apply` prints in its closing hint (same line as the Codex example in [`tmux/README.md` § Hook Setup](../tmux/README.md#hook-setup)). What matters is that the line is present and still invokes `agent-attention`.

Detect:

```bash
grep -n 'notify = .*agent-attention' ~/.codex/config.toml 2>/dev/null
```

Action:

If nothing prints, or the line differs from the closing hint, replace it with:

```toml
notify = ["/bin/sh", "-lc", "python3 \"$HOME/.config/tmux/scripts/agent-attention\" notify --source codex --event-type notify --title Codex"]
```

### 7. Wallpaper cache (Linux only)

The lock-screen rendering moved from `~/.cache/lock-screen/` into the wallpaper helper at `~/.cache/wallpaper/`. The old cache is orphaned but harmless.

Detect:

```bash
ls -la ~/.cache/lock-screen 2>/dev/null
du -sh ~/.cache/lock-screen 2>/dev/null
```

Action:

```bash
rm -rf ~/.cache/lock-screen
# The new cache at ~/.cache/wallpaper/ will be (re)populated next time
# `wallpaper set/use` runs or on next `lock-screen` invocation.
```

### 8. niri → sway (Linux only)

If a Linux machine was running niri, this is a desktop-stack switch, not a config update — see [`DECISIONS.md` § niri](./DECISIONS.md) for the rationale and [Sway School](https://martintrojer.github.io/sway-school/) for a tree-first sway tutorial.

Bringing the actual session up:

```bash
# Install sway and friends if not already present:
cd ~/dotfiles && ./fedora/setup-sway.sh    # Fedora
# (No automated path on other distros — see fedora/sway-packages.sh for the list.)

# Log out of niri, pick "Sway" at the display manager (or start sway from a TTY).
# Verify the WM is sway:
echo $XDG_CURRENT_DESKTOP    # expect: sway
swaymsg -t get_version       # expect: a sway version string
```

Old niri config under `~/.config/niri/` is harmless to leave in place — sway doesn't read it. Remove it if you want a clean slate:

```bash
rm -rf ~/.config/niri
```

### 9. macOS-specific cleanup

macOS machines should run items 1 through 6 above as relevant. Only the wallpaper/niri items are Linux-only no-ops on macOS.

Terminal policy on macOS is: **prefer Alacritty, tolerate Ghostty as fallback**. `dotfiles-sync --apply` stows both `alacritty` (common + Darwin overlay) and the small `ghostty` config on Darwin, because the active Macs are not perfectly uniform. Hammerspoon's terminal binds (`Hyper+T`, `Hyper+Return`, `Hyper+PadEnter`) try Alacritty first and fall back to Ghostty if Alacritty is absent.

Verify terminal state:

```bash
cd ~/dotfiles && ./dotfiles-sync --check
ls -l ~/.config/alacritty/alacritty.toml ~/.config/alacritty/os.toml 2>/dev/null
ls -l ~/.config/ghostty/config 2>/dev/null
```

Expected policy:

- New / primary Macs should install Alacritty and use that as the Hammerspoon target.
- Machines that only have Ghostty should still be usable; the fallback is intentional.
- Do not delete `ghostty/` just because a given Mac uses Alacritty — it is the compatibility config for mixed machines.

After the above, the Mac is current.

Verify overall sync:

```bash
cd ~/dotfiles && ./dotfiles-sync --check
# Expect: no issues other than any --ignore-listed unclassified packages.
```

## Update flow

When any of the agent-side content changes in this repo:

- **Skills and Pi extensions:** nothing to do. The `~/.agents/skills/<name>` and `~/.pi/agent/extensions/<name>.ts` symlinks point straight at the repo source; edits propagate live.
- **New / removed skills or Pi extensions:** re-run `./dotfiles-sync --apply` to create new symlinks or prune stale ones.
- **Claude (any change to `agents/`, `hooks/`, `skills/`, or `.claude-plugin/`):** push to `origin`, then run `claude plugin install mtrojer@dotfiles` on each consumer machine. (The marketplace add from the fresh-install step is one-time — only the `plugin install` needs to be re-run.)

## Testing and debugging the bootstrap

Two recipes. Different intents:

- **Recipe 1** (`--target=` against a temporary path): a fast dry-run of the install path. No container, no shell, just inspect what `--apply` would write into a fake `$HOME`.
- **Recipe 2** (podman with fake `$HOME`): an interactive debug shell with `--apply` already done. Use when something is broken and you want to poke at it on a clean machine.

Neither recipe exercises the manual Claude/Codex follow-ups. The Claude/Pi/Codex CLIs resolve home-relative paths internally and would still touch your real `~/.claude/`, `~/.pi/`, etc. — plus they're not in the bare `fedora:latest` image. If you want to test that flow, install the CLIs first inside the container, run the two `claude plugin ...` commands from the post-apply hint, and add the Codex `notify = [...]` line by hand inside the container.

### Recipe 1: `--target` against a temporary path (5 seconds)

```bash
rm -rf /tmp/fresh-home && mkdir /tmp/fresh-home
./dotfiles-sync --target=/tmp/fresh-home --apply
# Inspect:
ls -la /tmp/fresh-home/
ls /tmp/fresh-home/.agents/skills/
ls /tmp/fresh-home/.pi/agent/extensions/
ls /tmp/fresh-home/.local/share/zsh-plugins/
```

Validates everything `--apply` does — stow output, zsh-plugin clones, TPM clone, skill + Pi-extension symlinks — against a real path on disk. No container, no isolation: your shell still sees its real `$HOME` for everything else.

### Recipe 2: throwaway podman container with a fake `$HOME`

When recipe 1 isn't enough and you need a clean machine to reproduce a problem, drop into a podman container with `HOME` pointed at tmpfs:

```bash
podman run --rm -it \
  --tmpfs /home/test:exec,mode=0755 \
  -w /home/test \
  -e HOME=/home/test -e USER=test \
  --security-opt label=disable \
  -v "$PWD":/dotfiles:ro \
  fedora:latest \
  bash -c 'dnf -y install stow python3 git zsh >/dev/null && cp -r /dotfiles ~/dotfiles && cd ~/dotfiles && exec bash'
```

What the flags do, and why:

- `--tmpfs /home/test:exec,mode=0755` — fake `$HOME` lives in tmpfs, vanishes when container exits. `exec` is needed because `--apply` clones zsh plugins there and they include shell scripts that get sourced.
- `--security-opt label=disable` — skip SELinux relabeling on the bind mount. Without this, Fedora hosts (rpm-ostree at `/var/home/...`) refuse to read the mount because the host's SELinux label doesn't match. Alternative is `:Z` on the volume, but that *relabels the host directory in place* — a real persistent change to your real `~/dotfiles`. `label=disable` confines the SELinux loss to the throwaway container.
- `-v "$PWD":/dotfiles:ro` — your repo, read-only. We `cp -r` it to `~/dotfiles` inside the container so `--apply` can write symlinks freely without touching the host.
- `dnf -y install stow python3 git zsh` — the bare minimum: stow + python3 for `--apply`, git for the zsh-plugin clones, zsh so you can `exec zsh -l` afterwards and verify the rendered `.zshrc` actually loads. Add more (`fzf zoxide eza ripgrep fd-find tmux curl mise`) if you want to test more of the daily user experience.

From the container shell, run `./dotfiles-sync --apply` to do the actual stow work, then `exec zsh -l` to drop into the rendered shell.

## When the upgrade section can be deleted

The "Upgrading from an older setup" section above has a finite lifespan. When all your machines are current and you can't remember the last time anyone ran any of the steps in that section, delete it. The `DECISIONS.md` entries already capture *why* each thing changed; the upgrade section only exists to bridge the *how* for in-flight machines.
