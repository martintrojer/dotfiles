# Dotfiles

Personal dotfiles, deployed via [GNU Stow](https://www.gnu.org/software/stow/) and a thin Python driver (`stow-all.py`).

## Zen Of This Setup

The pillars below govern every package in this repo. They exist so future-me can resist the urge to chase the next shiny compositor release, plugin framework, or distro flavor and instead keep editing actual files.

When tempted, re-read this section before touching anything.

1. **Boring infra is good infra.** None of the tools here are exciting in 2026. That is the point. The desktop is the substrate, not the hobby.
2. **Builtins first, plugins last.** A plugin or external tool only lands when a builtin genuinely cannot do the job.
3. **Every line is understood.** No framework magic, no hidden keymaps, no "distro" config layers. If a line is here, future-me can explain why; if not, it gets deleted.
4. **Each piece earns its place.** Every plugin, package, script, and service answers a one-line "why not builtin?" question. Inertia is not an answer.
5. **Local scripts over upstream plugins.** A small Python script in this repo beats a third-party dependency. Easier to read, easier to fix, doesn't break on upgrade.
6. **Recreate, do not restore.** No session snapshots. No magic state restoration. Disposable sessions force the setup to stay cheap to spin up.
7. **Thin wrappers around shared lists.** Decisions live in data, not in scripts. `stow-all.py`, `setup-*.sh`, the package lists — all wrappers around plain data.
8. **Opinionated, not agnostic.** Linux is Fedora + Wayland + Sway. macOS is Hammerspoon + Ghostty. The shared layer is the CLI/editor baseline; the desktop stack is allowed to diverge per platform.
9. **One palette, everywhere.** Catppuccin Mocha. See [`THEME.md`](./THEME.md). New tools adopt the palette or do not get added.
10. **Config lives next to the thing it configures.** Tool-specific docs go in the package folder. This root README only describes the repo shape and the rules above.

If a new toy violates more than one of these, it does not belong here — no matter how cool the blur effect is.

## Quick start

```bash
git clone https://github.com/martintrojer/dotfiles ~/dotfiles
cd ~/dotfiles
./stow-all.py --apply --install-agents
```

Full install steps, the update flow, recipes for testing changes without clobbering your real `$HOME`, and cleanup steps for machines running an older version of this repo all live in [`SETUP.md`](./SETUP.md).

## Repository shape

The shared layer is intentionally the CLI/editor baseline. Desktop behaviour is allowed to diverge by platform.

| Where | What |
|---|---|
| Portable core | [`zsh/`](./zsh), [`nvim/`](./nvim), [`tmux/`](./tmux), [`git/`](./git), [`ssh/`](./ssh), [`local-bin/`](./local-bin) |
| Linux desktop stack | [`sway/`](./sway), [`waybar/`](./waybar), [`fuzzel/`](./fuzzel), [`kanshi/`](./kanshi), [`mako/`](./mako), [`swaylock/`](./swaylock) |
| macOS desktop stack | [`hammerspoon/`](./hammerspoon), [`ghostty/`](./ghostty) |
| Fedora setup | [`fedora/`](./fedora) (shared package lists + setup wrappers) |
| Agent payloads | [`skills/`](./skills), [`agents/`](./agents), [`hooks/`](./hooks), [`pi/extensions/`](./pi/extensions), [`.claude-plugin/`](./.claude-plugin) — see [Agent payloads](#agent-payloads) below for the deployment model |

Each top-level directory is a Stow package with its own `README.md` covering the tool-specific story. Start in the package folder when debugging or extending that tool.

## Agent payloads

The repo doubles as a multi-target agent plugin. Distribution model:

- **Universal (handled by `--apply`):** `skills/<name>/` and `pi/extensions/*.ts` are plain symlinks into `~/.agents/skills/` and `~/.pi/agent/extensions/` respectively. Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw all read these paths natively. Edits in the repo show up live.
- **Claude (handled by `--install-agents`):** github marketplace + plugin install. Re-runs only when the repo HEAD has advanced past the cached snapshot SHA.

Why this split: `~/.agents/skills/` is the universal path *all* the agents already read, so a plain symlink covers everyone except Claude in one move. Only Claude wants its own plugin cache, so it gets the github marketplace treatment.

See [`SETUP.md`](./SETUP.md) for the install + update flow.

## Decisions and rejected alternatives

Past audits — chezmoi, oh-my-zsh, niri→sway, the recurring TPM vendoring temptation, the centralized `docs/` folder — live in [`DECISIONS.md`](./DECISIONS.md). Read before relitigating.
