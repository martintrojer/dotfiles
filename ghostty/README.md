# Ghostty

Ghostty is kept as a Darwin-only compatibility fallback, not the preferred terminal target.

Policy:

- Prefer Alacritty on new / primary Macs.
- Keep this config stowed on Darwin so Macs that only have Ghostty remain usable.
- Do not add new workflow features that require Ghostty. Put shared terminal behavior in tmux/zsh/nvim, and keep app-specific config limited to rendering/input/clipboard/window-launch details.

Hammerspoon's terminal binds try Alacritty first and fall back to Ghostty when Alacritty is absent.
