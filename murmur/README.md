# murmur

Config for [murmur](https://github.com/mu-crew/murmur), the agent-state tool behind the tmux status bar (see [`tmux/README.md`](../tmux/README.md#ai-agent-attention)).

## Notifications

`.config/murmur/on-attention` is murmur's notification hook. murmur runs it once for each new `done`, `blocked`, `error` or `crashed` event, local or from a peer, and puts the event in `MURMUR_*` variables (listed in murmur's `docs/setup.md`).

| OS | Sends with | Urgency |
| --- | --- | --- |
| Linux | `notify-send -a murmur` (mako) | `done` is normal; `blocked` / `error` / `crashed` are critical and stay until dismissed |
| macOS | `osascript display notification` | no urgency levels |

- A `done` from a crew (orchestrated) agent is skipped. The orchestrator handles it.
- Events reach the hook from `murmur collect`, which runs on every status-bar tick, so a notification lands within one tick. Only the machine you sit at needs the hook.
- Test it: `MURMUR_KIND=blocked MURMUR_AGENT=test MURMUR_HOST=$(hostname) ~/.config/murmur/on-attention`.
