# Dotfiles

Personal dotfiles deployed by `dotfiles-sync`, the repo-specific sync tool under [`_dotfiles_sync/`](./_dotfiles_sync). Each top-level package mirrors `$HOME`, and the planner symlinks it into `$HOME` without an external dependency.

## Zen Of This Setup

These rules govern every package in the repo. They keep the setup focused on useful configuration instead of new compositor releases, plugin frameworks, or distro variants.

When tempted, re-read this section before touching anything.

1. **Boring infra is good infra.** None of the tools here are exciting in 2026. That is the point. The desktop supports the work; maintaining it is not the hobby.
2. **Builtins first, plugins last.** A plugin or external tool only lands when a builtin cannot do the job.
3. **Every line is understood.** No framework magic, no hidden keymaps, no "distro" config layers. If a line is here, future-me can explain why; if not, it gets deleted.
4. **Each piece earns its place.** Every plugin, package, script, and service answers a one-line "why not builtin?" question. Inertia is not an answer.
5. **Borrow ideas, not ecosystems.** Treat a good plugin or desktop stack as a reference. Reimplement only the useful part with local scripts and native APIs. A small Python script in this repo is easier to read, fix, and keep stable across upgrades than a third-party framework.
6. **Recreate, do not restore.** No session snapshots or hidden state restoration. Disposable sessions force the setup to remain cheap to start.
7. **Thin wrappers around shared lists.** Decisions live in data, not scripts. `dotfiles-sync` and `setup-*.sh` are wrappers around plain package lists.
8. **Opinionated, not agnostic.** Linux is Fedora + Wayland + Sway with foot as the terminal. macOS is Hammerspoon with Ghostty as the terminal. The shared layer is the CLI/editor baseline. Each desktop uses its OS's native primitives: Spaces and Mission Control on macOS; sway IPC and Wayland layer-shell on Linux; foot's server/client and Ghostty's app-bundle launch model on their respective systems.
9. **One palette, one layout language.** Catppuccin Mocha is the color system;
   blocky/tmux geometry is the interaction grammar. Blocks are affordances, not
   decoration: use them for navigation, focus, modal state, or problems that
   need a response, and keep ambient context quieter. See [`docs/THEME.md`](./docs/THEME.md)
   and [`docs/LAYOUT.md`](./docs/LAYOUT.md).
10. **Config lives next to the thing it configures.** Tool-specific docs go in the package folder. This root README only describes the repo shape and the rules above.
11. **Human-made, human-owned, heavily tested.** As in rule 1, "boring" describes provenance as well as release cadence. Prefer infrastructure with accountable maintainers, a test suite, and a track record, such as tmux, zsh, sway, and sqlite. Do not put generated or agent-built infrastructure beneath the setup. This repo delegates work to agents, but its foundations stay human-owned: a bad script line is a bug, while a bad foundation can force a rebuild.

If a new tool violates more than one rule, it does not belong here.

## Quick start

```bash
git clone https://github.com/martintrojer/dotfiles ~/dotfiles
cd ~/dotfiles
./dotfiles-sync --apply
# Then follow the manual step it prints (Codex notify hook).
```

[`docs/SETUP.md`](./docs/SETUP.md) covers installation, updates, isolated tests, and cleanup on machines that run an older version of the repo.

## Repository shape

The shared layer is intentionally the CLI/editor baseline. Desktop behaviour is allowed to diverge by platform.

| Where | What |
|---|---|
| Portable core (packages) | [`zsh/`](./zsh), [`nvim/`](./nvim), [`tmux/`](./tmux), [`git/`](./git), [`ssh/`](./ssh), [`local-bin/`](./local-bin) |
| Linux desktop stack (packages) | [`sway/`](./sway), [`waybar/`](./waybar), [`fuzzel/`](./fuzzel), [`foot/`](./foot) (terminal), [`kanshi/`](./kanshi), [`mako/`](./mako), [`swaylock/`](./swaylock) |
| Linux gaming layer | [`fedora/gaming/`](./fedora/gaming) — quarantined, opt-out (`--skip-gaming`) stack for the main Windows→Linux gaming rig (Steam, gamescope, Sunshine, OptiScaler, MangoHud, GameMode, OpenRGB); see [`fedora/gaming/README.md`](./fedora/gaming/README.md) and [`docs/DECISIONS.md`](./docs/DECISIONS.md) |
| macOS desktop stack (packages) | [`hammerspoon/`](./hammerspoon), [`ghostty/`](./ghostty) (terminal) |
| Fedora setup namespace | [`fedora/`](./fedora) (special case: nested packages + setup wrappers) |
| Universal agent sources (packages) | [`skills/`](./skills), [`pi/`](./pi) |
| Repo control plane | [`dotfiles-sync`](./dotfiles-sync), [`_dotfiles_sync/`](./_dotfiles_sync) |
| Cross-cutting docs/policy | [`docs/`](./docs) — [`SETUP.md`](./docs/SETUP.md), [`DECISIONS.md`](./docs/DECISIONS.md), [`THEME.md`](./docs/THEME.md), [`LAYOUT.md`](./docs/LAYOUT.md), [`VSCODE.md`](./docs/VSCODE.md) |

Most top-level directories are packages mirroring `$HOME`. The notable exceptions are the Fedora namespace and the repo control-plane files. `skills/` and `pi/` are packages too (their inner `.agents/` and `.pi/` trees mirror `$HOME`), but they double as source trees consumed directly by external tools.

## Agent payloads

The repo doubles as a multi-target agent source. Distribution model:

- **Handled by `--apply`:** `skills/.agents/skills/<name>/` and `pi/.pi/agent/extensions/*.ts` link into `~/.agents/skills/` and `~/.pi/agent/extensions/`. Codex, OpenCode, Pi, Cursor, Amp, Cline, Warp, OpenClaw all read these paths natively. Edits in the repo show up live. Skills are a `bundle_dirs` package so each skill links as one directory symlink (vendored README/LICENSE ride along); pi extensions link per-file. Pi helper modules (such as `_lib.ts`) must export a harmless default because pi may auto-load them.

Why this works: `~/.agents/skills/` is the universal path *all* the agents already read, so linking there covers everyone in one move.

See [`docs/SETUP.md`](./docs/SETUP.md) for the install + update flow.

## Decisions and rejected alternatives

[`docs/DECISIONS.md`](./docs/DECISIONS.md) records past audits of chezmoi, oh-my-zsh, niri→sway, TPM vendoring, and the centralized `docs/` folder. Read it before reopening those decisions.
