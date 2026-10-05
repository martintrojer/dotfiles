# murmur

Config for [murmur](https://github.com/mu-crew/murmur), the agent-state tool behind the tmux status bar (see [`tmux/README.md`](../tmux/README.md#ai-agent-attention)).

## Notifications

`.config/murmur/on-attention` is murmur's notification hook. murmur runs it once for each new `done`, `blocked`, `error` or `crashed` event, local or from a peer, and puts the event in `MURMUR_*` variables (listed in murmur's `docs/setup.md`).

| OS | Sends with | Urgency |
| --- | --- | --- |
| Linux | `notify-send -a murmur` (mako) | `done` is normal; `blocked` / `error` / `crashed` are critical and stay until dismissed |
| macOS | `terminal-notifier` with the icon, else `osascript display notification` | no urgency levels |

- Each notification carries a 128px icon. The frame is the host's accent and the glyph is the event kind in its color, all from murmur (`MURMUR_HOST_COLOR`, `MURMUR_GLYPH`, `MURMUR_KIND_COLOR`). murmur hashes the host color exactly as the tmux hostname pill does (`tmux/.config/tmux/scripts/status-hostname`), so a host is one color in both. The host name in the body is in the same accent.
- Icons render once per color pair and glyph with ImageMagick (`magick`) and the JetBrains Mono Nerd Font, cached in `~/.cache/murmur/`. Without `magick`, or with a murmur older than the host-colors release, the notification goes out without an icon.
- mako shows the icons at 72px (`[app-name=murmur]` in the mako config).

- A `done` from a crew (orchestrated) agent is skipped. The orchestrator handles it.
- Events reach the hook from `murmur collect`, which runs on every status-bar tick, so a notification lands within one tick. Only the machine you sit at needs the hook.
- Test it: `MURMUR_KIND=blocked MURMUR_AGENT=test MURMUR_HOST=$(hostname) ~/.config/murmur/on-attention`.
