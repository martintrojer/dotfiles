#!/bin/bash

niri_packages=(
  # Primary terminal used by the Niri setup.
  alacritty
  # Clipboard history backend used by the fuzzel picker and startup watcher.
  clipman
  # Containerized dev environments launched from the desktop session.
  distrobox
  # Quick system summary tool for shell sessions.
  fastfetch
  # Primary graphical web browser for the desktop session.
  google-chrome-stable
  # Icon font used by Waybar and related desktop UI.
  fontawesome-fonts-all
  # Output profile manager for monitors and docking changes.
  kanshi
  # Notification daemon for the Wayland session.
  mako
  # Terminal UI for managing NetworkManager connections.
  NetworkManager-tui
  # Wayland compositor used by this setup.
  niri
  # Media key integration for playback control.
  playerctl
  # Calculator backend for launcher workflows.
  qalculate
  # Syncs files to remote storage from the desktop session.
  rclone
  # Wallpaper process for the Niri session.
  swaybg
  # Idle manager for dim, lock, and suspend behavior.
  swayidle
  # Lock screen used by the idle flow.
  swaylock
  # Status bar for the desktop session.
  waybar
  # Audio mixer TUI launched from Waybar.
  wiremix
  # Types into Wayland apps from scripts and keybindings.
  wtype
  # Application launcher used across desktop scripts.
  fuzzel
)
