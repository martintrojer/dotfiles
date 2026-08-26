#!/bin/bash
set -euo pipefail

# Install the SteamOS-style gamescope session (steam.desktop: 4K HDR couch PC
# gaming, launcher defaults) so it can be picked from the SDDM login screen. An
# embedded/DRM gamescope session that execs the linked launcher. See
# fedora/gaming/docs/GAMESCOPE-SESSION.md.
#
# It wires system paths (writable + persistent on Atomic via the
# /usr/local -> /var/usrlocal symlink), which SDDM already searches
# (SessionDir=/usr/local/share/wayland-sessions,...):
#
#   /usr/local/bin/steam-session           -> the linked launcher
#   /usr/local/bin/steamos-session-select  -> the linked exit shim
#   /usr/local/share/wayland-sessions/steam.desktop   couch HDR entry
#
# The exit shim goes in /usr/local/bin (always on PATH) rather than relying on
# ~/.local/bin being inherited: the gamepad UI's "Desktop Mode" runs
# steamos-session-select via a PATH we don't control, so we put it where the
# system PATH already reaches. See fedora/gaming/docs/GAMESCOPE-SESSION.md.
#
# Prereqs: steam + gamescope layered (os/setup-steam.sh) and the dotfiles linked
# (dotfiles-sync --apply), so ~/.local/bin/steam-session exists. Run as your
# normal user; it uses sudo for the system paths.

script_dir=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)

launcher="$HOME/.local/bin/steam-session"
session_select="$HOME/.local/bin/steamos-session-select"
desktop_src="$script_dir/gamescope-session/steam.desktop"

for f in "$launcher" "$session_select"; do
  if [[ ! -x "$f" ]]; then
    echo "error: $f not found or not executable." >&2
    echo "Run 'dotfiles-sync --apply' first to link ~/.local/bin." >&2
    exit 1
  fi
done

if [[ ! -f "$desktop_src" ]]; then
  echo "error: $desktop_src not found." >&2
  exit 1
fi

sudo install -d -m 0755 /usr/local/bin /usr/local/share/wayland-sessions
sudo ln -sf "$launcher" /usr/local/bin/steam-session
sudo ln -sf "$session_select" /usr/local/bin/steamos-session-select
sudo install -m 0644 "$desktop_src" \
  "/usr/local/share/wayland-sessions/$(basename "$desktop_src")"

echo "Installed the 'Steam (gamescope)' session."
echo "Installed exit shim at /usr/local/bin/steamos-session-select."
echo "Pick a session at the SDDM login screen; use gamepad UI 'Desktop Mode' to return to SDDM."
