#!/bin/bash
set -euo pipefail

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
manifest="$script_dir/mise/.config/mise/config.toml"
mise="$HOME/.local/bin/mise"

if ! command -v mise >/dev/null && [[ ! -x "$mise" ]]; then
  curl -fsSL https://mise.run | sh
fi

if command -v mise >/dev/null; then
  mise=$(command -v mise)
fi

# Use the tracked global config directly. Shell activation already lives in
# zsh/.zsh/tools.zsh, and installing does not require it.
(
  cd /tmp
  MISE_GLOBAL_CONFIG_FILE="$manifest" "$mise" install --yes
)
