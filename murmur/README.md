# murmur

Config for [murmur](https://github.com/mu-crew/murmur), the agent-state tool behind the tmux status bar (see [`tmux/README.md`](../tmux/README.md#ai-agent-attention)).

## Sounds

`.config/murmur/on-attention` is murmur's attention hook. It plays one sound per event instead of a desktop notification; the tmux pill and `murmur dash` already show who needs you. The sound rises with severity:

| Kind | macOS (`afplay`, `/System/Library/Sounds`) | Fedora (`pw-play`, sound-theme-freedesktop) |
|---|---|---|
| done | Bottle | complete |
| blocked | Glass | window-attention |
| error | Basso | dialog-error |
| crashed | Submarine | suspend-error |

A crew agent's `done` stays silent: that is the orchestrator's business.

Test it: `MURMUR_KIND=blocked ~/.config/murmur/on-attention`
