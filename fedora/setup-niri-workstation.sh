#!/bin/bash

# https://copr.fedorainfracloud.org/coprs/acidburnmonkey/hyprland/
# https://copr.fedorainfracloud.org/coprs/scottames/ghostty/

dnf copr enable -y acidburnmonkey/hyprland
dnf copr enable -y scottames/ghostty

dnf install -y \
  clipman \
  fastfetch \
  fontawesome-fonts-all \
  ghostty \
  hypridle \
  hyprlock \
  hyprpaper \
  kanshi \
  mako \
  niri \
  playerctl \
  qalculate \
  waybar \
  wiremix \
  wl-clipboard \
  fuzzel \
  xdg-terminal-exec
