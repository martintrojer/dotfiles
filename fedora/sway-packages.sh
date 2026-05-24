#!/bin/bash

# Packages layered on top of Fedora Sway Atomic (Sericea) for this setup.
# Directly used packages intentionally left to the Sericea base image:
# sway, foot, kanshi, swaybg, swayidle, swaylock, waybar, wl-clipboard,
# grim, slurp, jq, brightnessctl, playerctl, pipewire, xdg-desktop-portal-wlr,
# and xdg-desktop-portal-gtk.
sway_packages=(
  # Session/UI pieces not already shipped by the Sericea base image.
  # Application launcher used across desktop scripts.
  fuzzel
  # Icon font used by Waybar and related desktop UI.
  fontawesome-fonts-all
  # Notification daemon for the Wayland session.
  mako

  # Desktop workflow apps.
  # Containerized dev environments launched from the desktop session.
  distrobox
  # Primary graphical web browser for the desktop session.
  google-chrome-stable

  # Picker and Waybar backends.
  # Clipboard history backend used by the fuzzel picker and startup watcher.
  clipman
  # Terminal UI for managing NetworkManager connections.
  NetworkManager-tui
  # Calculator backend for launcher workflows.
  qalculate
  # Audio mixer TUI launched from Waybar.
  wiremix
  # Types into Wayland apps from scripts and keybindings.
  wtype
)
