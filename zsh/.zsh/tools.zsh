# Initialize mise (version manager)
if command -v mise >/dev/null; then
  eval "$(mise activate zsh)"
elif test -e "${HOME}/.local/bin/mise"; then
  eval "$(~/.local/bin/mise activate zsh)"
fi

# Initialize other tools
test -e "${HOME}/.ghcup/env" && . "${HOME}/.ghcup/env"

autoload -Uz colors && colors
setopt prompt_subst

prompt_dir_color="%F{#b4befe}"
prompt_accent_color="%F{#f9e2af}"
prompt_error_color="%F{#f38ba8}"
prompt_muted_color="%F{#6c7086}"
prompt_toolbox_color="%F{#94e2d5}"
prompt_reset="%f"
prompt_error_glyph="󰅚"
prompt_context=""

PROMPT="${prompt_dir_color}%3~${prompt_reset} ${prompt_accent_color}❯${prompt_reset} "
# Right-aligned prompt: show a failure marker plus subtle environment context.
if [[ -f /run/.toolboxenv && -r /run/.containerenv ]]; then
  prompt_toolbox_name="$(grep -E '^name="' /run/.containerenv 2>/dev/null | cut -d '"' -f 2)"
  prompt_context="${prompt_toolbox_color}${prompt_toolbox_name:-toolbox}${prompt_reset}"
fi

if [[ -n "${SSH_CONNECTION:-}" ]]; then
  prompt_context="${prompt_context:+${prompt_context} }${prompt_muted_color}%m${prompt_reset}"
fi

RPROMPT="%(?..${prompt_error_color}${prompt_error_glyph}${prompt_reset})"
if [[ -n "${prompt_context}" ]]; then
  RPROMPT="%(?..${prompt_error_color}${prompt_error_glyph}${prompt_reset} )${prompt_context}"
fi
