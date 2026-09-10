export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PYTORCH_ENABLE_MPS_FALLBACK=1

# Homebrew's gdu ships as `gdu-go` to avoid a conflict with `gdu` (the GNU
# du from coreutils). Alias it back to `gdu` when present.
[ -e "/opt/homebrew/bin/gdu-go" ] && alias gdu='gdu-go'
