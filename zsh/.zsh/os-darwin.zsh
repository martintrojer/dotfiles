export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export PYTORCH_ENABLE_MPS_FALLBACK=1

# cursor-agent: Keychain auth often fails to persist across launches.
# File store writes tokens to ~/.cursor/auth.json instead.
export AGENT_CLI_CREDENTIAL_STORE=file
