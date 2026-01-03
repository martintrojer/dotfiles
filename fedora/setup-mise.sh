#!/bin/bash

# Install mise if not already installed
if ! command -v mise &> /dev/null; then
  curl https://mise.run | sh
  # Add mise to PATH for current session
  export PATH="$HOME/.local/bin:$PATH"
fi

eval "$(mise activate bash)"

# Install tools via mise
mise use \
  bat \
  eza \
  fzf \
  glow \
  lazydocker \
  lazygit \
  neovim \
  node \
  starship \
  tokei \
  tree-sitter \
  uv \
  yazi \
  github:danielmiessler/Fabric@latest \
  github:zk-org/zk@latest \
  zoxide

