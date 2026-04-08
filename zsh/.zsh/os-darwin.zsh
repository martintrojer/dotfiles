export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PYTORCH_ENABLE_MPS_FALLBACK=1

if test -e "/opt/homebrew/bin/brew"; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
elif test -e "/usr/local/bin/brew"; then
  eval "$(/usr/local/bin/brew shellenv)"
fi

if [ -z "${lesspipe_path:-}" ] && [ -f "/opt/homebrew/bin/lesspipe.sh" ]; then
  lesspipe_path="/opt/homebrew/bin/lesspipe.sh"
fi

if [ -n "${lesspipe_path:-}" ] && [ -n "${bat_cmd:-}" ]; then
  export LESSOPEN="| $lesspipe_path %s"
  export LESS="-R"
  export LESSCOLORIZER="$bat_cmd"
fi
