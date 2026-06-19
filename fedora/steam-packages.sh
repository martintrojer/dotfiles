#!/bin/bash

# Gaming/Steam packages layered on top of Fedora Sway Atomic (Sericea).
#
# These require extra repos that are NOT part of the COPR-free host baseline,
# so they must be enabled before running setup-steam.sh:
#
# 1. RPM Fusion (free + nonfree) for steam, gamescope, mangohud, gamemode:
#      rpm-ostree install \
#        https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
#        https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
#
# 2. LACT COPR for AMD GPU control (lact):
#      https://copr.fedorainfracloud.org/coprs/ilyaz/LACT/
#      -> drop the repo file at /etc/yum.repos.d/lact.repo
#      after install: sudo systemctl enable --now lactd
steam_packages=(
  # Steam client and controller/device udev rules.
  steam
  steam-devices
  # Micro-compositor for upscaling/scaling and per-game overlays.
  gamescope
  # In-game performance overlay.
  mangohud
  # AMD GPU control daemon (fan curves, power limits); needs the LACT COPR.
  lact
  # RGB lighting control (so it can run system-wide, e.g. turn off at boot).
  openrgb
)
