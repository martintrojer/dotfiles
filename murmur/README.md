# murmur

Config for [murmur](https://github.com/mu-crew/murmur), the agent-state tool behind the tmux status bar (see [`tmux/README.md`](../tmux/README.md#ai-agent-attention)).

## Notifications

`.config/murmur/on-attention` is murmur's notification hook. It is a shim: the hook itself is mu-crew/dotfiles' `murmur/on-attention`, from the clone dotfiles-sync pins in `_dotfiles_sync/pins.py`. See [its README](https://github.com/mu-crew/dotfiles#notifications) for what it sends.

- Each notification has an icon: the host's accent as the frame, around the event kind's glyph in its color. The host color is the same as the tmux hostname pill (`@mu_crew_host_color`).
- mako shows the icons at 72px (`[app-name=murmur]` in the mako config).
- Test it: `MURMUR_KIND=blocked MURMUR_AGENT=test MURMUR_HOST=$(hostname) ~/.config/murmur/on-attention` (no icon: murmur normally supplies its colors and glyph).
