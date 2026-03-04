# Tmux

tmux needs a manual step to install plugins:

1. `git clone https://github.com/tmux-plugins/tpm $HOME/.tmux/plugins/tpm`
2. [inside tmux] `<CTRL>b I`

## AI Agent Attention

This package includes `$HOME/.config/tmux/scripts/agent-attention`, a lightweight event collector and tmux UI integration for agent harnesses.

What it does:

- marks windows with pending agent attention as `[!]`
- shows total flagged windows in `status-right` as `AI:<count>`
- opens a picker (`prefix + A`) listing flagged windows, then jumps to the selected one
- clears attention automatically when you visit that window
- sends desktop notifications on macOS/Linux by default (set `TMUX_AGENT_ATTENTION_DISABLE_SYSTEM_NOTIFY=1` to disable)

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
        "hooks": [
          {
            "type": "command",
            "command": "python3 $HOME/.config/tmux/scripts/agent-attention notify --source claude --event-type notification --title Claude"
          }
        ],
        "matcher": ""
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
