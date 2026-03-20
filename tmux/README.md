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
- `fcsonline/tmux-thumbs`: hint-based picking inside visible pane content, similar to Vimium-style jump labels for paths, URLs, SHAs, numbers, and other matches.
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

## Using tmux-thumbs

`tmux-thumbs` is a fast hint picker for useful text visible in the current tmux pane, such as URLs, paths, SHAs, numbers, and other tokens.

Default flow:

- `prefix` + `Space`: start `tmux-thumbs`
- type the shown hint in lowercase: copy the match into the tmux paste buffer
- type the final hint character in uppercase: copy and paste immediately into the active pane

If you used lowercase copy, paste it with normal tmux buffer commands:

- `prefix` + `]`: paste the most recent tmux buffer
- `prefix` + `=`: open the tmux buffer list and choose one to paste

## Using tmux-fzf

`tmux-fzf` is a general fuzzy finder for tmux objects rather than visible pane text.

Default flow:

- `prefix` + `F`: open `tmux-fzf`
- use it to search sessions, windows, panes, key bindings, clipboard buffers, and other tmux actions

Notes:

- This is complementary to `tmux-thumbs`: `tmux-fzf` is for tmux state and management, while `tmux-thumbs` is for picking text from pane content.
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

- `$HOME/.cache/tmux-agent-attention/pending.jsonl`

## Hook Setup (manual, outside stow)

Point each harness to:

- `$HOME/.config/tmux/scripts/agent-attention notify`

### Claude Code (`$HOME/.claude/settings.json`)

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

### Codex CLI (`$HOME/.codex/config.toml`)

```toml
notify = "python3 $HOME/.config/tmux/scripts/agent-attention notify --source codex --event-type notify --title Codex"
```

### OpenCode (`$HOME/.config/opencode/plugin/notify.ts`)

```typescript
import type { Plugin } from "@opencode-ai/plugin";

const NOTIFY_SCRIPT = `${process.env.HOME}/.config/tmux/scripts/agent-attention`;

const plugin: Plugin = async ({ client, $ }) => {
  const notify = (type: string, message: string) => {
    const payload = JSON.stringify({ source: "opencode", type, title: "OpenCode", message });
    $`echo ${payload} | python3 ${NOTIFY_SCRIPT} notify`.quiet().catch(() => {});
  };

  return {
    event: async ({ event }) => {
      if (event.type === "session.idle") {
        const { sessionID } = event.properties;
        const sessions = await client.session.list({ limit: 50 });
        const session = sessions.data?.find((s: { id: string }) => s.id === sessionID);
        if (!session || session.parentID) return;

        notify("session.idle", session.title || "Task completed");
      }

      if (event.type === "permission.asked") {
        const { permission, patterns } = event.properties;
        const detail = patterns.length ? `: ${patterns.join(", ")}` : "";
        notify("permission.asked", `${permission}${detail}`);
      }
    },
  };
};

export default plugin;
```

## Cheatsheet

`prefix + g` opens an fzf cheatsheet popup inside tmux. The entries are embedded in `.config/tmux/scripts/cheatsheet` — update that script when adding or changing keybindings.
