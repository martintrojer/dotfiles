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
mise use --global \
  bat@latest \
  btop@latest \
  eza@latest \
  fd@latest \
  fastfetch@latest \
  fzf@latest \
  gdu@latest \
  gh@latest \
  github:LuaLS/lua-language-server@latest \
  github:pythops/bluetui@latest \
  github:rgwood/systemctl-tui@latest \
  github:tekumara/typos-lsp@latest \
  github:zk-org/zk@latest \
  glow@latest \
  go@latest \
  jj@latest \
  just@latest \
  lazygit@latest \
  marp-cli@latest \
  neovim@latest \
  node@24 \
  prettier@latest \
  ripgrep@latest \
  rclone@latest \
  rust@latest \
  rust-analyzer@latest \
  shellcheck@latest \
  stylua@latest \
  tokei@latest \
  tree-sitter@latest \
  uv@latest \
  vale@latest \
  yazi@latest \
  zoxide@latest
