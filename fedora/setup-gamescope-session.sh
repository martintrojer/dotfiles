#!/bin/bash
set -euo pipefail

# Install the "Steam (gamescope)" SteamOS-style session so it can be picked from
# the SDDM login screen. This is the embedded/DRM gamescope session (true HDR);
# see fedora/docs/HDR-GAMING.md.
#
# It wires two system paths (writable + persistent on Atomic via the
# /usr/local -> /var/usrlocal symlink), which SDDM already searches
# (SessionDir=/usr/local/share/wayland-sessions,...):
#
#   /usr/local/bin/steam-session                  -> the stowed launcher
#   /usr/local/share/wayland-sessions/steam.desktop  the session entry
#
# Prereqs: steam + gamescope layered (setup-steam.sh) and the dotfiles stowed
# (dotfiles-sync --apply), so ~/.local/bin/steam-session exists. Run as your
# normal user; it uses sudo for the system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

launcher="$HOME/.local/bin/steam-session"
desktop_src="$script_dir/gamescope-session/steam.desktop"

if [[ ! -x "$launcher" ]]; then
  echo "error: $launcher not found or not executable." >&2
  echo "Run 'dotfiles-sync --apply' first to stow ~/.local/bin." >&2
  exit 1
fi

if [[ ! -f "$desktop_src" ]]; then
  echo "error: $desktop_src not found." >&2
  exit 1
fi

sudo install -d -m 0755 /usr/local/bin /usr/local/share/wayland-sessions
sudo ln -sf "$launcher" /usr/local/bin/steam-session
sudo install -m 0644 "$desktop_src" /usr/local/share/wayland-sessions/steam.desktop

echo "Installed 'Steam (gamescope)' session."
echo "Pick it at the SDDM login screen; quit Steam to return to SDDM."
