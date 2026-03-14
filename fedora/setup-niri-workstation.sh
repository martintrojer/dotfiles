#!/bin/bash

# https://copr.fedorainfracloud.org/coprs/acidburnmonkey/hyprland/

dnf copr enable -y acidburnmonkey/hyprland

dnf install -y \
  clipman \
  fastfetch \
  fontawesome-fonts-all \
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
