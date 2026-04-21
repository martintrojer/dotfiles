export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PYTORCH_ENABLE_MPS_FALLBACK=1

if [ -z "${lesspipe_path:-}" ] && [ -f "/opt/homebrew/bin/lesspipe.sh" ]; then
  lesspipe_path="/opt/homebrew/bin/lesspipe.sh"
fi

if [ -n "${lesspipe_path:-}" ] && [ -n "${bat_cmd:-}" ]; then
  export LESSOPEN="| $lesspipe_path %s"
  export LESS="-R"
  export LESSCOLORIZER="$bat_cmd"
fi
