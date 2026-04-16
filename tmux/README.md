# Tmux

tmux needs a manual step to install plugins:

1. `git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm`
2. [inside tmux] `<CTRL>b I`

## Plugin Inventory

This config uses TPM plus a mix of quality-of-life, persistence, navigation, and status-line plugins.

- `tmux-plugins/tpm`: tmux plugin manager. It installs, updates, and loads the rest of the plugins from `.tmux.conf`.
- `tmux-plugins/tmux-sensible`: conservative tmux defaults. It provides a small baseline of settings that are meant to be broadly useful without overriding your explicit config.
- `tmux-plugins/tmux-yank`: copies from tmux into the system clipboard. Most useful in copy mode and for pushing text out of tmux into the desktop clipboard.
- `tmux-plugins/tmux-resurrect`: saves and restores tmux sessions, windows, panes, layouts, and some running programs.
- `tmux-plugins/tmux-cpu`: provides CPU usage segments for the status line.
- `Morantron/tmux-fingers`: hint-based picking inside visible pane content, similar to Vimium-style jump labels for paths, URLs, SHAs, numbers, and other matches.
- `sainnhe/tmux-fzf`: fzf-powered tmux management for sessions, windows, panes, bindings, clipboard history, and process actions.
- `catppuccin/tmux`: the Catppuccin theme integration that drives the Mocha status line styling and the rounded window/tab appearance.
- `christoomey/vim-tmux-navigator`: seamless navigation between Neovim splits and tmux panes with the same control-key motions.

## What You Actually Use Here

From your current `tmux/.tmux.conf`:

- The visible "power bar" look is mainly `catppuccin/tmux`.
- Vim split to tmux pane movement comes from `christoomey/vim-tmux-navigator`.
- The right side CPU segment comes from `tmux-cpu`.
- Session save and restore comes from `tmux-resurrect`.
- The `agent-attention` integration is not a plugin. It is a local script in this repo that adds `[!]` markers and the popup picker.

## Using Resurrect

`tmux-resurrect` gives you manual session save and restore.

Default keys:

- `prefix` + `Ctrl-s`: save the current tmux state
- `prefix` + `Ctrl-r`: restore the last saved tmux state

What it restores well:

- sessions, windows, panes, and layouts
- pane working directories
- some long-running programs, depending on plugin support and program behavior

Practical workflow:

1. Arrange your tmux sessions, windows, and panes the way you want.
2. Press `prefix` + `Ctrl-s` to save.
3. Later, start tmux and press `prefix` + `Ctrl-r` to restore.

Notes:

- This is manual restore only. There is no auto-restore now that `tmux-continuum` has been removed.
- Restoring tmux structure is more reliable than restoring arbitrary application state.
- If a restored program does not come back exactly as expected, the pane layout and working directory are usually still restored correctly.

## Using tmux-fingers

`tmux-fingers` is a fast hint picker for useful text visible in the current tmux pane, such as URLs, paths, SHAs, numbers, and other tokens.

Configured flow:

- `prefix` + `Space`: start `tmux-fingers`
- `prefix` + `j`: start `tmux-fingers` in jump mode, which moves the cursor to the selected match
- type the shown hint to copy the match to the clipboard and tmux buffer
- type `Shift` + the final hint character to copy and paste immediately into the active pane

Custom patterns carried over from the previous setup:

- email addresses
- `host:port`
- semantic versions
- `D123`-style identifiers
- `T123`-style identifiers

If you copy without the shift-paste action, paste it with normal tmux buffer commands:

- `prefix` + `]`: paste the most recent tmux buffer
- `prefix` + `=`: open the tmux buffer list and choose one to paste

## Using tmux-fzf

`tmux-fzf` is a general fuzzy finder for tmux objects rather than visible pane text.

Default flow:

- `prefix` + `F`: open `tmux-fzf`
- use it to search sessions, windows, panes, key bindings, clipboard buffers, and other tmux actions

Notes:

- This is complementary to `tmux-fingers`: `tmux-fzf` is for tmux state and management, while `tmux-fingers` is for picking text from pane content.
- Your config makes the popup larger than the plugin default with `TMUX_FZF_OPTIONS="-p -w 80% -h 75% -m"`.
- The `-m` flag enables multi-select in pickers that support it.

## AI Agent Attention

This package includes `$HOME/.config/tmux/scripts/agent-attention`, a lightweight event collector and tmux UI integration for agent harnesses.

What it does:

- marks windows with pending agent attention as `[!]`
- shows total flagged windows in `status-right` as `AI:<count>`
- opens a picker (`prefix + A`) listing flagged windows, then jumps to the selected one
- clears attention automatically when you visit that window
- sends desktop notifications on macOS/Linux by default (set `TMUX_AGENT_ATTENTION_DISABLE_SYSTEM_NOTIFY=1` to disable)
- emits OSC 777 terminal notifications to the controlling TTY (for terminals that support it)

Runtime state is stored in:

- `$XDG_STATE_HOME/tmux-agent-attention/pending.jsonl` (default `~/.local/state/tmux-agent-attention/pending.jsonl`)

## Hook Setup

Each agent harness calls `agent-attention notify --source <name>` when it needs attention. `stow-all.py` checks all four hooks and auto-creates symlinks for Pi and OpenCode. Claude Code and Codex require manual config.

Run `stow-all.py -c -v` to see which hooks are configured and which are missing.

### Claude Code (manual — `$HOME/.claude/settings.json`)

Add to the `hooks` key:

```json
{
  "hooks": {
    "Notification": [
      {
        "matcher": "",
        "hooks": [
          {
            "type": "command",
            "command": "python3 $HOME/.config/tmux/scripts/agent-attention notify --source claude --event-type notification --title Claude"
          }
        ]
      }
    ]
  }
}
```

### Codex CLI (manual — `$HOME/.codex/config.toml`)

Add to the top level:

```toml
notify = ["python3", "$HOME/.config/tmux/scripts/agent-attention", "notify", "--source", "codex", "--event-type", "notify", "--title", "Codex"]
```

### OpenCode (auto — `stow-all.py --apply`)

Symlinked by `stow-all.py` from `tmux/.config/tmux/scripts/opencode-plugin/notify.ts` to `~/.config/opencode/plugin/notify.ts`.

Subscribes to `session.idle` and `permission.asked` events.

### Pi Agent (auto — `stow-all.py --apply`)

Symlinked by `stow-all.py` from `tmux/.config/tmux/scripts/pi-extensions/agent-attention.ts` to `~/.pi/agent/extensions/agent-attention.ts`.

Subscribes to Pi's `turn_end` event.

## Cheatsheet

`prefix + g` opens an fzf cheatsheet popup inside tmux. The entries are embedded in `.config/tmux/scripts/cheatsheet` — update that script when adding or changing keybindings.
