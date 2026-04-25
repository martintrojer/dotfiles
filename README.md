# Dotfiles
Intended to be used with [GNU Stow](https://www.gnu.org/software/stow/)

## Zen Of This Setup

The pillars below are the same ones that already govern `nvim/`, `tmux/`, and `fedora/`. They exist so future-me can resist the urge to chase the next shiny compositor release, plugin framework, or distro flavor and instead keep editing actual files.

When tempted, re-read this section before touching anything.

1. **Boring infra is good infra.** Fedora rpm-ostree, Sway, tmux, Neovim, zsh. None of these are exciting in 2026. That is the point. The desktop should be the substrate, not the hobby.
2. **Builtins first, plugins last.** Neovim 0.12 builtins (LSP, completion, comments, snippets, `vim.pack`) before any plugin. Native tmux status formatting, `choose-tree`, popups, and menus before TPM plugins. Standard Fedora repos before COPRs. A plugin or external tool only lands when a builtin genuinely cannot do the job.
3. **Every line is understood.** No framework magic, no hidden keymaps, no "distro" config layers. If a line of config is here, future-me can explain why. If it cannot be explained, it gets deleted.
4. **Each piece earns its place.** Plugins, packages, scripts, services — they all justify themselves with a one-line "why not builtin?" answer. The `nvim/README.md` plugin table is the template for how to think about every other tool too.
5. **Local scripts over upstream plugins.** `tms`, `agent-attention`, `status-*`, `cheatsheet`, `lock-screen`, the fuzzel pickers, `stow-all.py` — small Python scripts in this repo are preferred over adding a third-party dependency. They are easy to read, easy to fix, and they do not break on upgrade.
6. **Recreate, do not restore.** No tmux resurrect. No session snapshots. No magic state restoration. The workflow is rebuilt on demand: `tms` recreates project sessions in two keystrokes, `mini.starter` and `<leader>fo` re-enter files, agents keep their own state. Disposable sessions force the setup to stay cheap to spin up.
7. **Thin wrappers around shared lists.** `fedora/setup-*.sh` are wrappers around `base-packages.sh` / `sway-packages.sh`. `stow-all.py` is a wrapper around stow + clone-on-apply for the two zsh plugins + plain symlinks for agent skills (`~/.agents/skills/`) and pi extensions (`~/.pi/agent/extensions/`). The Claude plugin is the one outlier (it wants its own plugin cache, so we install via `claude plugin marketplace add martintrojer/dotfiles`). Decisions live in data, not in scripts.
8. **Opinionated, not agnostic.** Linux is Fedora + Wayland + Sway. macOS is Hammerspoon + Ghostty. The shared layer is the CLI/editor baseline; the desktop stack is allowed to diverge per platform. No effort is spent making the WM/compositor portable.
9. **One palette, everywhere.** Catppuccin Mocha across nvim, tmux, eza, bat, waybar, fuzzel, mako, swaylock. See `THEME.md`. New tools adopt the palette or do not get added.
10. **Config lives next to the thing it configures.** Tool-specific docs go in the package folder (`nvim/README.md`, `tmux/README.md`, `fedora/README.md`, `fuzzel/README.md`, ...). This root README only describes the repo shape and the rules above.

If a new toy violates more than one of these, it does not belong here — no matter how cool the blur effect is.

## Repository Shape

- Portable core: shell, editor, git, terminal tooling
- Linux desktop stack: Fedora + Wayland + `sway`
- macOS desktop stack: Hammerspoon + Ghostty

The shared layer is intentionally the CLI/editor baseline. Desktop behavior is allowed to diverge by platform.

## Stow

Use `./stow-all.py` to stow the packages that match the current OS and distro.

Not everything at the repo root is a stow package. In particular, [`skills/`](./skills), [`agents/`](./agents), [`commands/`](./commands), [`hooks/`](./hooks), [`pi/extensions/`](./pi/extensions), and [`.claude-plugin/`](./.claude-plugin) are agent-side payloads consumed by `claude plugin` and the universal `~/.agents/skills/` / `~/.pi/agent/extensions/` paths rather than stowed into `~/` directly.

## Agent Plugin (skills, extensions, commands, hooks)

The whole repo doubles as a multi-target agent plugin. The distribution model splits into two halves:

**Universal (handled by `--apply`, no flag needed):** skills and pi extensions are plain symlinks into the repo.

- `dotfiles/skills/<name>/` → `~/.agents/skills/<name>` (one symlink per skill). Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw and friends all read `~/.agents/skills/` natively. Edits in the repo show up live (no copy).
- `dotfiles/pi/extensions/*.ts` → `~/.pi/agent/extensions/`. Pi auto-discovers TS files there and supports `/reload`.

Stale links (skills you removed from the repo) are pruned automatically.

**Claude Code (handled by `--install-agents`, opt-in):** Claude wants its own plugin cache, so it gets the github marketplace treatment.

- `claude plugin marketplace add martintrojer/dotfiles`
- `claude plugin install mtrojer@dotfiles`

### Install on a fresh machine

```bash
git clone https://github.com/martintrojer/dotfiles ~/dotfiles
cd ~/dotfiles
./stow-all.py --apply --install-agents
# Then add the codex notify line to ~/.codex/config.toml (printed by --apply).
```

`--apply` always:
- Stows the dotfile packages.
- Clones the pinned zsh plugins.
- Symlinks `dotfiles/skills/*` into `~/.agents/skills/` and `dotfiles/pi/extensions/*.ts` into `~/.pi/agent/extensions/`.

`--install-agents` adds:
- Claude marketplace registration (one-time).
- Claude plugin install/refresh (re-runs only when the repo's git HEAD has advanced past the cached snapshot SHA).

Without `--install-agents`, `--apply` prints the exact two `claude` commands you'd need to run by hand.

### Update flow

When any of the agent-side content changes in this repo:

- **For skills and pi extensions:** nothing to do. The `~/.agents/skills/<name>` and `~/.pi/agent/extensions/<name>.ts` symlinks point straight at the repo source; edits propagate live.
- **For new skills or removed skills:** re-run `./stow-all.py --apply` to create new symlinks or prune stale ones.
- **For Claude (any change to `agents/`, `commands/`, `hooks/`, or `skills/`):** push to `origin`, then re-run `./stow-all.py --apply --install-agents` on each consumer machine. The plugin install only re-fetches when the repo HEAD has actually advanced past the cached snapshot.

### Why this layout

Previous incarnations of `stow-all.py` carried ~600 lines of code to fan-out skills into 4 different agent dirs, copy a Claude plugin bundle, symlink Pi extensions, and verify Claude/Codex notification hooks. The simplification: `~/.agents/skills/` is the universal path *all* the agents already read, so a plain symlink covers Codex/OpenCode/Pi/Cursor/Amp/Cline/Warp/OpenClaw at once. Same for `~/.pi/agent/extensions/`. Only Claude needed special treatment.

Key Linux packages now include:
- `sway`
- `waybar`
- `fuzzel`
- `kanshi`
- `mako`
- `swaylock`

Wallpapers themselves are not stowed; the `wallpaper` helper in `fedora/bin/.local/bin/` manages them per machine under `~/.local/share/wallpapers/`.

Fedora-specific setup lives under `fedora/`, with shared package definitions in:
- `fedora/base-packages.sh`
- `fedora/sway-packages.sh`

Other notable shared packages:
- `tmux/` for tmux config, local session picker/launcher scripts, and status helpers
- `zsh/` for shell config and the `tm` helper that launches the local tmux session picker
- `local-bin/` for shared `~/.local/bin` commands across macOS and Linux

Fedora-only command wrappers live in `fedora/bin/`.

## Platform Stance

Linux in this repo is intentionally opinionated: Fedora + Wayland + `sway`.
The goal is not to keep the desktop stack compositor-agnostic; the goal is to keep the Sway workflow cohesive and maintainable.

## OS Conditionals

Keep OS detection at explicit boundaries such as package selection, bootstrap scripts, and backend helpers.
Avoid embedding platform branches directly in otherwise shared user-facing config when a wrapper script or sourced OS-specific file would be clearer.

## Considered And Rejected

Ideas that look attractive on first read but failed the pillar test on inspection. **Do not relitigate without new evidence.**

### chezmoi (rejected 2026-04-25)

Audited [chezmoi](https://www.chezmoi.io/) v2.70.2 against `stow-all.py`'s actual responsibilities. Findings:

- chezmoi cleanly replaces the boring half of `stow-all.py` (stow driver + conflict UX + OS scoping). `chezmoi diff` / `chezmoi apply --dry-run` are nicer than the custom `--check` flow.
- chezmoi does **not** solve the harder half:
  - Per-skill symlinks into `~/.agents/skills/` don't fit the static-target-state model. Either you duplicate symlink declarations or you drop into `run_onchange_*.sh` scripts — same Python code, just relocated under a different roof.
  - Files mutated by other tools (`~/.claude/settings.json`, `~/.codex/config.toml`) don't fit chezmoi's "chezmoi owns the target" model.
- **Pillar costs:** violates pillar #2 (chezmoi is a heavier abstraction than stow + Python), pillar #3 (filename munging like `dot_`, `private_`, `executable_`, `symlink_`, `run_onchange_` plus Go template syntax = more concepts to hold in head, not fewer), pillar #10 (source tree no longer mirrors `$HOME` shape; today `nvim/.config/nvim/init.lua` is its own destination, under chezmoi it becomes `dot_config/nvim/init.lua` and the repo loses its self-documenting layout).
- **Features not used today:** templating (no per-machine variation in `git/.gitconfig` etc.), encryption (secrets are intentionally out-of-repo per `AGENTS.md`), the externals fetcher. chezmoi's most valuable features would sit unused on day one.

**Conclusion:** chezmoi solves the *easy* half of the problem and ignores the *hard* half. The right response to "`stow-all.py` is sprawling" was to delete the agent-orchestration code in favor of upstream tools and `~/.agents/skills/` symlinks (already done), not to swap the underlying tool.

**Reconsider only if:** chezmoi gains a real per-skill fan-out primitive (one source file → N symlink destinations), *or* you start needing per-machine templated configs and in-repo secrets at scale.

## Notes
Project-specific setup details should live in the corresponding package folder
(e.g. `tmux/README.md`, `nvim/README.md`, etc.), not in this root README.
