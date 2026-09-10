# Linux-only shell tweaks. Sourced early from .zshrc (like os-darwin.zsh)
# so TERM is settled before the modular ~/.zsh/*.zsh load.

# Toolbox containers often lack the host's terminfo entry (foot,
# tmux-256color, ...). Force a known-good value inside toolbox.
if [[ -f /run/.toolboxenv ]]; then
  export TERM=xterm-256color
fi

# Electron on Wayland/X11: prefer the compositor's Ozone backend.
export ELECTRON_OZONE_PLATFORM_HINT=auto

# Called from tools.zsh after theme colors exist. No-op outside toolbox.
apply_linux_prompt_context() {
  [[ -f /run/.toolboxenv && -r /run/.containerenv ]] || return 0
  local name
  name="$(grep -E '^name="' /run/.containerenv 2>/dev/null | cut -d '"' -f 2)"
  prompt_context="${prompt_toolbox_color}${name:-toolbox}${prompt_reset}"
}
