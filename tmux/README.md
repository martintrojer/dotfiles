# Tmux

tmux needs a manual step to install plugins:

1. `git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm`
2. [inside tmux] `<CTRL>b I`

This setup expects `sesh` for the repo-defined session flows in `tmux/.tmux.conf`, such as `prefix + s`, `prefix + g`, and any other local sesh binds you keep enabled.

## Plugin Inventory

This config uses TPM plus a mix of quality-of-life, persistence, navigation, and status-line plugins.

- `tmux-plugins/tpm`: tmux plugin manager. It installs, updates, and loads the rest of the plugins from `.tmux.conf`.
- `tmux-plugins/tmux-yank`: copies from tmux into the system clipboard. Most useful in copy mode and for pushing text out of tmux into the desktop clipboard.
- `tmux-plugins/tmux-resurrect`: saves and restores tmux sessions, windows, panes, layouts, and some running programs.
- `tmux-plugins/tmux-cpu`: provides the `#{cpu_percentage}` format used by the native status bar's CPU segment.
- `Morantron/tmux-fingers`: hint-based picking inside visible pane content, similar to Vimium-style jump labels for paths, URLs, SHAs, numbers, and other matches.
- `sainnhe/tmux-fzf`: fzf-powered tmux management for sessions, windows, panes, bindings, clipboard history, and process actions.
- `christoomey/vim-tmux-navigator`: seamless navigation between Neovim splits and tmux panes with the same control-key motions.

## What You Actually Use Here

From your current `tmux/.tmux.conf`:

- The visible status bar is native tmux formatting using named Catppuccin Mocha palette variables defined near the top of `.tmux.conf`.
- Built-in tmux UI surfaces such as `choose-tree`, menus, popups, and prompts are also styled directly with Catppuccin Mocha hex values instead of the stock tmux colors.
- Vim split to tmux pane movement comes from `christoomey/vim-tmux-navigator`.
- The right side CPU segment comes from `tmux-cpu`.
- Cross-platform RAM usage is provided by `$HOME/.config/tmux/scripts/status-ram`.
- Session save and restore comes from `tmux-resurrect`.
- The `agent-attention` integration is not a plugin. It is a local script in this repo that adds `[!]` markers and the popup picker.
- Cross-platform uptime is provided by `$HOME/.config/tmux/scripts/status-uptime`.
- Window labels are derived from the active pane by `$HOME/.config/tmux/scripts/status-window-label`, so vertical-split workflows can switch between labels like `nvim`, `claude`, `π - ...`, or a cwd basename.
- The AI badge itself is rendered by `$HOME/.config/tmux/scripts/status-ai`.

## Status Bar Layout

The current bar keeps the same useful information as before, but without the pill-style Catppuccin theme chrome.

- Left: a filled session block.
- Center: merged window labels (`number + active-pane label`) with a filled active window, flat inactive windows, and inline `!` / `Z` markers.
- Pane, window, and session switches trigger an immediate `refresh-client -S`, so label changes show up right away instead of waiting for the status timer.
- Right: boxed `PREFIX` and `AI` segments, followed by flatter glyph-based `CPU`, `RAM`, `host`, and `uptime` segments.

## Built-in Tmux UI

Native tmux pickers and overlays use the same palette as the status bar instead of the default yellow-accent tmux theme.

Repo-defined bindings in the current `tmux/.tmux.conf`:

- `prefix` + `s`: `sesh` picker popup
- `prefix` + `S`: tmux `choose-tree` session picker, sorted by name
- `prefix` + `N`: `tmux-fzf` new session flow
- `prefix` + `g`: switch to last session via `sesh`
- `prefix` + `!`: break the current pane out into a new window
- `prefix` + `M`: move the current pane into the selected window or pane as a split
- `prefix` + `w`: built-in tmux session-window tree picker
- `prefix` + `Ctrl-g`: cheatsheet popup
- mouse click on the left status session block: opens the tmux session picker
- built-in menus, prompts, and popups use Mocha background/foreground colors with a sky selection highlight

Mental model for pane moving:

- `prefix` + `!`: split current pane away from its window. Current pane becomes a new one-pane window.
- `prefix` + `M`: pick destination in tmux tree, then insert current pane there as a split. Source window loses that pane.
- In short: `!` means "pull this pane out"; `M` means "move this pane into there".

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

- `prefix` + `h`: start `tmux-fingers`
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

- `prefix` + `s` opens the local popup-backed `sesh` picker script.
- the sesh picker uses `-d -s` so duplicates are hidden and separator matching feels better
- `prefix` + `N` jumps straight into `tmux-fzf`'s new-session flow.
- `prefix` + `g` keeps `sesh last` on an easy key without colliding with your existing tmux binds.
- `prefix` + `w` remains tmux's standard session-window tree picker.
- `prefix` + `Ctrl-g` moves the cheatsheet off a prime lowercase key.
- This is complementary to `tmux-fingers`: `tmux-fzf` is for tmux state and management, while `tmux-fingers` is for picking text from pane content.
- Your config makes the popup larger than the plugin default with `TMUX_FZF_OPTIONS="-p -w 80% -h 75% -m"`.
- The `-m` flag enables multi-select in pickers that support it.

## AI Agent Attention

This package includes `$HOME/.config/tmux/scripts/agent-attention`, a lightweight event collector and tmux UI integration for agent harnesses.

What it does:

- marks windows with pending agent attention as `[!]`
- shows total flagged windows in `status-right` as a boxed `AI <count>` segment
- opens a picker (`prefix + A`) listing flagged windows, then jumps to the selected one
- clears attention automatically when you focus the agent's pane (window-level visit alone is not enough, so vertical splits behave correctly)
- when a notify event fires, the badge is only suppressed if the user is *already* focused on the agent's pane; same-window-but-different-pane still queues the badge
- sends desktop notifications on macOS/Linux by default (set `TMUX_AGENT_ATTENTION_DISABLE_SYSTEM_NOTIFY=1` to disable)
- emits OSC 777 terminal notifications to the controlling TTY (for terminals that support it)

Runtime state is stored in:

- `$XDG_STATE_HOME/tmux-agent-attention/pending.jsonl` (default `~/.local/state/tmux-agent-attention/pending.jsonl`)

## Hook Setup

Each agent harness calls `agent-attention notify --source <name>` when it needs attention. The script resolves the current tmux target from `--pane`, then `TMUX_PANE`, then the controlling TTY, which makes hook subprocesses more reliable. `stow-all.py` checks all four hooks and auto-creates symlinks for Pi and OpenCode. Claude Code and Codex require manual config.

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
notify = ["/bin/sh", "-lc", "python3 \"$HOME/.config/tmux/scripts/agent-attention\" notify --source codex --event-type notify --title Codex"]
```

The `/bin/sh -lc` wrapper is important here because Codex's TOML array form does not expand `$HOME` by itself.

### OpenCode (auto — `stow-all.py --apply`)

Symlinked by `stow-all.py` from `tmux/.config/tmux/scripts/opencode-plugin/notify.ts` to `~/.config/opencode/plugin/notify.ts`.

Subscribes to `session.idle` and `permission.asked` events.

### Pi Agent (auto — `stow-all.py --apply`)

Symlinked by `stow-all.py` from `tmux/.config/tmux/scripts/pi-extensions/agent-attention.ts` to `~/.pi/agent/extensions/agent-attention.ts`.

Subscribes to Pi's `agent_end` event so attention only fires once the prompt is actually finished.

## Cheatsheet

`prefix + Ctrl-g` opens an fzf cheatsheet popup inside tmux. The entries mix repo-defined bindings with stock tmux and plugin defaults, so update that script when adding, removing, or reclassifying keys.
