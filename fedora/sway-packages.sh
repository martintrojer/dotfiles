#!/bin/bash

sway_sericea_base_packages=(
  # Fedora Sericea/Sway Atomic base packages.
  #
  # Keep these in the shared Sway list so mutable Fedora installs and
  # non-Sericea rpm-ostree variants still get a complete desktop session.
  # On Sericea they should show up as inactive requested packages if layered.
  brightnessctl
  grim
  kanshi
  playerctl
  slurp
  sway
  swaybg
  swayidle
  swaylock
  waybar
  wl-clipboard
)

sway_extra_packages=(
  # Primary terminal used by the Sway setup.
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
  # Notification daemon for the Wayland session.
  mako
  # Terminal UI for managing NetworkManager connections.
  NetworkManager-tui
  # Calculator backend for launcher workflows.
  qalculate
  # Syncs files to remote storage from the desktop session.
  rclone
  # Audio mixer TUI launched from Waybar.
  wiremix
  # Types into Wayland apps from scripts and keybindings.
  wtype
  # Application launcher used across desktop scripts.
  fuzzel
)

sway_packages=(
  "${sway_sericea_base_packages[@]}"
  "${sway_extra_packages[@]}"
)
