#!/usr/bin/env bash
# Cloud Agent bootstrap for this dotfiles repo.
#
# The repo has no application to run; its development experience is the local
# gate `make check-all` (see docs/SETUP.md § "Pinned toolchain"). The pinned
# checkers are fetched on demand by uv/npx, so this script only provisions the
# ambient tools the base image lacks:
#
#   - uv       drives the Python side (interpreter, ruff, ty)
#   - fd       the Makefile calls `fd`; Debian ships it as `fdfind`
#   - zsh      `make check-zsh` runs `zsh -n` on the shell fragments
#   - luacheck a Lua rock with no usable npm/PyPI build (per the Makefile note)
#   - a modern Node (>= 22.19) for `make check-ts` / `check-ts-tests`
#   - pi + opencode host packages that `make check-ts` type-checks against
#
# It is idempotent: every step is guarded or safe to repeat.
set -euo pipefail

# pi's published API tracks its npm release; pin the version this repo's
# extensions are written against so `make check-ts` stays reproducible.
PI_VERSION=0.85.1
OPENCODE_PLUGIN_VERSION=1.18.30

log() { printf '==> %s\n' "$*"; }

log 'apt: zsh, fd, luarocks toolchain'
export DEBIAN_FRONTEND=noninteractive
sudo apt-get update -qq
sudo apt-get install -y -qq \
	zsh \
	fd-find \
	luarocks \
	lua5.4 \
	liblua5.4-dev \
	build-essential

# The Makefile invokes `fd`; Debian/Ubuntu install the binary as `fdfind`.
if ! command -v fd >/dev/null 2>&1; then
	log 'link fd -> fdfind'
	sudo ln -sf "$(command -v fdfind)" /usr/local/bin/fd
fi

# luacheck has no usable npm/PyPI distribution, so build the Lua rock.
if ! command -v luacheck >/dev/null 2>&1; then
	log 'luarocks install luacheck'
	sudo luarocks install luacheck
fi

# uv fetches and caches the pinned Python interpreter, ruff, and ty.
if ! command -v uv >/dev/null 2>&1; then
	log 'install uv into /usr/local/bin'
	curl -LsSf https://astral.sh/uv/install.sh |
		sudo env UV_INSTALL_DIR=/usr/local/bin UV_UNMANAGED_INSTALL=/usr/local/bin sh
fi

# The base image's default `node` shim is 22.14: too old for pi (needs
# >= 22.19) and for `node --test *.test.ts`, which relies on built-in type
# stripping (default from 22.18). nvm ships a suitable LTS; activate it and put
# it ahead of the shim so `make` and the agent's shell both use it.
export NVM_DIR="$HOME/.nvm"
set +u
# shellcheck disable=SC1091
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
nvm use --silent default
set -u
export PATH="$NVM_BIN:$PATH"

BASHRC="$HOME/.bashrc"
MARKER='# dotfiles cloud-agent: prefer the nvm default node over the base shim'
if ! grep -qF "$MARKER" "$BASHRC" 2>/dev/null; then
	log 'pin nvm default node ahead of the base shim in ~/.bashrc'
	# Quoted heredoc: the block is written verbatim, so $HOME/$NVM_BIN expand
	# in the agent's shell at login, not here.
	cat >>"$BASHRC" <<'BASHRC_BLOCK'

# dotfiles cloud-agent: prefer the nvm default node over the base shim
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
if command -v nvm >/dev/null 2>&1; then
	nvm use --silent default >/dev/null 2>&1 || true
	[ -n "${NVM_BIN:-}" ] && export PATH="$NVM_BIN:$PATH"
fi
BASHRC_BLOCK
fi

# `make check-ts` type-checks the pi extensions and the opencode plugin against
# their host packages. link_ts_types.py symlinks pi from the global npm root
# and the opencode SDK from ~/.config/opencode into a scratch node_modules.
log "install pi coding agent $PI_VERSION (global npm root)"
npm install -g "@earendil-works/pi-coding-agent@${PI_VERSION}"

log "install opencode plugin SDK $OPENCODE_PLUGIN_VERSION"
mkdir -p "$HOME/.config/opencode"
(cd "$HOME/.config/opencode" && npm install "@opencode-ai/plugin@${OPENCODE_PLUGIN_VERSION}")

# `make check-tmux-tests` drives the real status scripts against an isolated
# tmux server that reads the ambient ~/.tmux.conf (it needs `base-index 1`).
# Linking the tmux package with the repo's own planner supplies that config and
# doubles as a smoke test of `dotfiles-sync --apply`. Scoped to tmux so the
# agent's git identity, ssh, and Cursor hooks are left untouched.
log 'link the tmux package (dotfiles-sync --apply tmux)'
./dotfiles-sync --apply tmux

log 'bootstrap complete'
