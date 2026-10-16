export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PYTORCH_ENABLE_MPS_FALLBACK=1

# cursor-agent: Keychain auth often fails to persist across launches.
# File store writes tokens to ~/.cursor/auth.json instead.
export AGENT_CLI_CREDENTIAL_STORE=file

# Homebrew's gdu ships as `gdu-go` to avoid a conflict with `gdu` (the GNU
# du from coreutils). Alias it back to `gdu` when present.
[ -e "/opt/homebrew/bin/gdu-go" ] && alias gdu='gdu-go'
