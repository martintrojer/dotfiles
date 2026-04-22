#!/bin/bash
set -euo pipefail

# Install mise if not already installed
if ! command -v mise &> /dev/null; then
  curl https://mise.run | sh
  # Add mise to PATH for current session
  export PATH="$HOME/.local/bin:$PATH"
fi

eval "$(mise activate bash)"

# Install tools via mise
mise use \
  bat@latest \
  eza@latest \
  fzf@latest \
  github:pythops/bluetui@latest \
  github:rgwood/systemctl-tui@latest \
  github:zk-org/zk@latest \
  glow@latest \
  go@latest \
  jj@latest \
  just@latest \
  lazygit@latest \
  neovim@latest \
  node@latest \
  prettier@latest \
  tokei@latest \
  tree-sitter@latest \
  uv@latest \
  vale@latest \
  yazi@latest \
  zoxide@latest
