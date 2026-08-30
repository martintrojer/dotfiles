# Setup

This guide covers fresh installs, upgrades, routine updates, and isolated tests
that do not modify your real `$HOME`.

Pick the section that matches your starting state:

- **Fresh machine** → [Fresh install](#fresh-install).
- **Existing machine on an older version of this repo** (OMZ-based zsh, manually-cloned TPM, per-agent skill copies, etc.) → [Upgrading from an older setup](#upgrading-from-an-older-setup).
- **Already current, just want to pull and re-apply changes** → [Update flow](#update-flow).
- **Hacking on `dotfiles-sync` itself** → [Testing and debugging the bootstrap](#testing-and-debugging-the-bootstrap).

## Quick start

```bash
git clone https://github.com/martintrojer/dotfiles ~/dotfiles
cd ~/dotfiles
./dotfiles-sync --apply
# Then run the manual command the script prints:
#   - Codex notify hook (one TOML line in ~/.codex/config.toml)
```

The sections below cover each step and the available upgrade paths.

## Fresh install

`./dotfiles-sync --apply` always:

- Links the dotfile packages that match the current OS and distro.
- Clones the pinned zsh plugins into `~/.local/share/zsh-plugins/`.
- Clones TPM (tmux plugin manager) into `~/.tmux/plugins/tpm/` at the pinned ref. Install the tmux plugins listed in `.tmux.conf` once with `prefix + I` inside tmux.
- Links `dotfiles/skills/.agents/skills/*` into `~/.agents/skills/` (the universal path read by Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw). Each skill lands as one directory symlink.
- Links `dotfiles/pi/.pi/agent/extensions/*.ts` into `~/.pi/agent/extensions/` (Pi auto-discovers these).
- Prunes stale links when you remove a skill or Pi extension from the repo.

After `--apply` completes, it prints one manual follow-up:

1. Add the printed `notify = [...]` line to `~/.codex/config.toml`. `--apply` does not rewrite the user's TOML for one line.

## Upgrading from an older setup

Use this section on machines that ran the OMZ-based zsh setup, stored per-agent skill copies, or cloned TPM manually. Each item has a detection command followed by an action. Run the action only when detection finds the old state. The items are independent.

### Step 0: pull the latest, run apply

```bash
cd ~/dotfiles
git pull
./dotfiles-sync --check    # surface link conflicts before they bite
./dotfiles-sync --apply
# (The universal ~/.agents/skills and ~/.pi/agent/extensions symlinks are
# already live after --apply.)
```

`--apply` is idempotent. It links new packages, including skills and Pi extensions, and clones the zsh plugins and TPM. The cleanup steps remove files from old layouts that `--apply` does not touch.

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

Older versions of the repo bootstrap (~1370 lines before the `_dotfiles_sync/` split, see commit `b29b3003`) copied each skill into per-agent locations: `~/.codex/skills/`, `~/.agents/skills/`, and others. The new model is a single set of symlinks at `~/.agents/skills/` that all supported agents read natively. Old per-agent copies are now stale — they won't get updates from the repo, and they may shadow the canonical symlinks.

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
# Expect each line to be: lrwxrwxrwx ... <name> -> .../dotfiles/skills/.agents/skills/<name>
```

### 4. Old Claude plugin install

This repo no longer ships a Claude Code plugin. If a machine still has the `mtrojer`/`dotfiles` Claude plugin installed from an earlier version of this repo, remove it.

Detect:

```bash
claude plugin list 2>/dev/null
# Look for any "mtrojer" or "dotfiles" plugin.
```

Action:

```bash
# Remove the stale install (replace <name> with whatever `claude plugin list` shows):
claude plugin uninstall <name>
```

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

### 6. Codex notify hook (repoint at murmur)

The hook invoked `agent-attention`, which no longer exists. murmur now has the
`notify` verb this section used to say was missing, so the line is repaired
rather than deleted: codex keeps its tmux attention.

Detect:

```bash
grep -n 'notify = .*agent-attention' ~/.codex/config.toml 2>/dev/null
```

Action:

If it prints, replace that line with:

```toml
notify = ["/bin/sh", "-lc", "murmur notify --source codex --event-type notify --title Codex"]
```

Verify it from inside a tmux pane, because a silent failure here is the whole
hazard — a notify hook's output goes nowhere:

```bash
/bin/sh -lc "murmur notify --source codex --event-type notify --title Codex"
murmur status                      # expect a blocked row for this pane
murmur clear --pane "$TMUX_PANE"   # then take it back
```

If that reports `murmur: not found`, the hook cannot find it either. `-l` does
not save you: `/bin/sh` is not zsh and never reads `.zprofile`, so the line works
only because it inherits the PATH of the terminal that launched codex. A codex
started by a launcher, daemon or GUI gets the default `sh` PATH, which has no
`/opt/homebrew`. Use the absolute path from `command -v murmur` in that case.

The opencode plugin at `opencode/.config/opencode/plugin/notify.ts` pipes the
same JSON payload to `murmur notify` and needs no separate setup.

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
cd ~/dotfiles && ./fedora/os/setup-sway.sh    # Fedora
# (No automated path on other distros — see fedora/os/sway-packages.sh for the list.)

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

Terminal policy on macOS is: **Ghostty only**, no fallback. `dotfiles-sync --apply` links the `ghostty` config on Darwin. Hammerspoon's terminal binds (`Hyper+T`, `Hyper+Return`, `Hyper+PadEnter`) all use `open -na "Ghostty"` for current-Space window creation. See [`DECISIONS.md`](./DECISIONS.md#each-os-gets-its-native-terminal-foot-on-linux-ghostty-on-macos-accepted-2026-05-15).

Verify terminal state:

```bash
cd ~/dotfiles && ./dotfiles-sync --check
ls -l ~/.config/ghostty/config 2>/dev/null
```

If migrating from a previous Alacritty-everywhere setup on this machine:

```bash
rm -rf ~/.config/alacritty   # safe: nothing in this repo references it anymore
```

After the above, the Mac is current.

Verify overall sync:

```bash
cd ~/dotfiles && ./dotfiles-sync --check
# Expect: no issues other than any --ignore-listed unclassified packages.
```

## Update flow

When any of the agent-side content changes in this repo:

- **Skills and Pi extensions:** nothing to do. The `~/.agents/skills/<name>` and `~/.pi/agent/extensions/<name>.ts` symlinks point straight at the repo source; edits propagate live.
- **New / removed skills or Pi extensions:** re-run `./dotfiles-sync --apply` to link new entries or prune stale ones.

## Pushing changes: `make push`

This repo has no hosted CI (see [`DECISIONS.md`](./DECISIONS.md)). The gate is local and opt-in:

```bash
make push                    # check-all, then jj git push
make push ARGS='-b main'     # extra args go through ARGS
```

`check-all` is a prerequisite of `push`, so a red check means make never reaches the push at all. Nothing lands on the remote.

**To bypass, run `jj git push` (or the `jjgp` alias) directly.** That is deliberate, not an oversight — the plain command is the escape hatch and `make push` is the gate you opt into. Use it when the failure is unrelated to what you're pushing (a missing local tool, say) and you'd rather not be blocked.

**Why a make target and not a pre-push hook.** jj (0.42) has no hook mechanism — no config key, nothing in `jj util config-schema`, nothing in the CLI — and `jj git push` does not run git's client-side hooks, because it pushes through its own git library rather than shelling out to `git`. Verified empirically against a throwaway colocated repo: a `.git/hooks/pre-push` that exits 1 blocks `git push` and is silently ignored by `jj git push`. Since jj is the primary VCS here, a pre-push hook would present as a gate while the command actually used walked straight past it. If jj ever gains hooks, this target becomes a one-line wrapper around one.

The gate can't leak into other repos: it's this repo's Makefile, with no global git config or `core.hooksPath` involved. `make check-all` needs tmux, zsh, luacheck, `uv`, `npm`/`node`, and network on first run — that toolchain requirement is the other half of why this stayed local.

### Pinned toolchain

Every checker version lives in the Makefile, not on the machine. `uv` runs the Python side (interpreter, `ruff`, `ty`) and `npx` the rest (`prettier`, `tsc`, `stylua`, plus `shellcheck` via the `shellcheck-py` wheel), so all of them fetch-and-cache the exact pinned version and `make check-all` behaves the same on every host. `make tool-versions` prints the current pins; bumping one is a one-line diff.

Two deliberate exceptions:

- **luacheck** stays a system binary. It's a Lua rock with no usable npm or PyPI distribution — the `luacheck` npm package is unrelated 2015-era bindings.
- **`target-version` in `ruff.toml` trails `PYTHON_VERSION`.** The checkers run under the pinned interpreter, but the scripts themselves are executed by each host's `#!/usr/bin/env python3`. Keeping the emitted syntax a release behind stops a linter bump from rewriting code into a form (e.g. PEP 758 `except A, B:`) that a not-yet-upgraded machine can't parse.

## Testing and debugging the bootstrap

Two recipes. Different intents:

- **Recipe 1** (`--target=` against a temporary path): a fast dry-run of the install path. No container, no shell, just inspect what `--apply` would write into a fake `$HOME`.
- **Recipe 2** (podman with fake `$HOME`): an interactive debug shell with `--apply` already done. Use when something is broken and you want to poke at it on a clean machine.

Neither recipe exercises the manual Codex follow-up. The Pi/Codex CLIs resolve home-relative paths internally and would still touch your real `~/.pi/`, `~/.codex/`, etc. — plus they're not in the bare `fedora:latest` image. If you want to test that flow, install the CLIs first inside the container and add the Codex `notify = [...]` line by hand inside the container.

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

Validates everything `--apply` does — the link plan, zsh-plugin clones, TPM clone, skill + Pi-extension symlinks — against a real path on disk. No container, no isolation: your shell still sees its real `$HOME` for everything else.

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
  bash -c 'dnf -y install python3 git zsh >/dev/null && cp -r /dotfiles ~/dotfiles && cd ~/dotfiles && exec bash'
```

What the flags do, and why:

- `--tmpfs /home/test:exec,mode=0755` — fake `$HOME` lives in tmpfs, vanishes when container exits. `exec` is needed because `--apply` clones zsh plugins there and they include shell scripts that get sourced.
- `--security-opt label=disable` — skip SELinux relabeling on the bind mount. Without this, Fedora hosts (rpm-ostree at `/var/home/...`) refuse to read the mount because the host's SELinux label doesn't match. Alternative is `:Z` on the volume, but that *relabels the host directory in place* — a real persistent change to your real `~/dotfiles`. `label=disable` confines the SELinux loss to the throwaway container.
- `-v "$PWD":/dotfiles:ro` — your repo, read-only. We `cp -r` it to `~/dotfiles` inside the container so `--apply` can write symlinks freely without touching the host.
- `dnf -y install python3 git zsh` — the bare minimum: python3 for `--apply` (the symlink planner is in-repo, no external tool), git for the zsh-plugin clones, zsh so you can `exec zsh -l` afterwards and verify the rendered `.zshrc` actually loads. Add more (`fzf zoxide eza ripgrep fd-find tmux curl mise`) if you want to test more of the daily user experience.

From the container shell, run `./dotfiles-sync --apply` to do the actual linking, then `exec zsh -l` to drop into the rendered shell.

## When the upgrade section can be deleted

The "Upgrading from an older setup" section above has a finite lifespan. When all your machines are current and you can't remember the last time anyone ran any of the steps in that section, delete it. The `DECISIONS.md` entries already capture *why* each thing changed; the upgrade section only exists to bridge the *how* for in-flight machines.
