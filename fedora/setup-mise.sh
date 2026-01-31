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
  github:danielmiessler/Fabric@latest \
  github:pythops/bluetui@latest \
  github:zk-org/zk@latest \
  glow \
  jj \
  lazydocker \
  lazygit \
  neovim \
  node \
  prettier \
  starship \
  tokei \
  tree-sitter \
  uv \
  vale \
  yazi \
  zoxide
