export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PYTORCH_ENABLE_MPS_FALLBACK=1

# Homebrew's gdu ships as `gdu-go` to avoid a conflict with `gdu` (the GNU
# du from coreutils). Alias it back to `gdu` when present.
[ -e "/opt/homebrew/bin/gdu-go" ] && alias gdu='gdu-go'

if [ -z "${lesspipe_path:-}" ] && [ -f "/opt/homebrew/bin/lesspipe.sh" ]; then
  lesspipe_path="/opt/homebrew/bin/lesspipe.sh"
fi

if [ -n "${lesspipe_path:-}" ] && [ -n "${bat_cmd:-}" ]; then
  export LESSOPEN="| $lesspipe_path %s"
  export LESS="-R"
  export LESSCOLORIZER="$bat_cmd"
fi
