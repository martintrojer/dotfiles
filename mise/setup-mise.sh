#!/bin/bash
set -euo pipefail

# Installs the global mise manifest on Linux and macOS: the shared
# ~/.config/mise/config.toml plus this OS's conf.d/ extras. Both are links
# that `dotfiles-sync --apply` creates, so run that first. mise reads conf.d/
# only beside the real global config, so this cannot point mise at the repo
# copy the way an override would.
script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)
manifest="$HOME/.config/mise/config.toml"
mise="$HOME/.local/bin/mise"

if [[ ! "$manifest" -ef "$script_dir/.config/mise/config.toml" ]]; then
  echo "error: $manifest is not linked to this repo; run ./dotfiles-sync --apply first" >&2
  exit 1
fi

if ! command -v mise >/dev/null && [[ ! -x "$mise" ]]; then
  curl -fsSL https://mise.run | sh
fi

if command -v mise >/dev/null; then
  mise=$(command -v mise)
fi

# Shell activation lives in zsh/.zsh/tools.zsh; installing does not need it.
(
  cd /tmp
  "$mise" install --yes
  "$mise" exec lua -- luarocks install --local luacheck 1.2.0-1
)
