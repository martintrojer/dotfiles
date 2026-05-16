#!/bin/bash

# Packages layered on top of Fedora Sway Atomic (Sericea) for this setup.
sway_packages=(
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
