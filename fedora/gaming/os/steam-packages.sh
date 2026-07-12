#!/bin/bash

# Gaming/Steam packages layered on top of Fedora Sway Atomic (Sericea).
#
# These require extra repos that are NOT part of the COPR-free host baseline,
# so they must be enabled before running os/setup-steam.sh:
#
# 1. RPM Fusion (free + nonfree) for steam, gamescope, mangohud, gamemode:
#      rpm-ostree install \
#        https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
#        https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
#
# 2. Sunshine COPR (beta) for Moonlight game streaming (Sunshine):
#      https://copr.fedorainfracloud.org/coprs/g/lizardbyte/beta/
#      -> drop the repo file at /etc/yum.repos.d/sunshine.repo
#      beta not stable: stable often lags the newest Fedora. The rpm is
#      Atomic-correct (caps ship as file capabilities, %post skips under
#      rpm-ostree, ships its own udev rule + user unit). The user unit is
#      started on demand by the gamescope stream session, NOT enabled. See
#      fedora/gaming/docs/STREAMING.md.
steam_packages=(
  # Steam client and controller/device udev rules.
  steam
  steam-devices
  # Micro-compositor for upscaling/scaling and per-game overlays.
  gamescope
  # In-game performance overlay.
  mangohud
  # RGB lighting control (so it can run system-wide, e.g. turn off at boot).
  openrgb
  # Moonlight game-stream host for the gamescope stream session; needs the
  # Sunshine COPR. Started on demand by steam-session (GS_SUNSHINE=1). Run
  # config/setup-sunshine.sh afterwards to open the firewall ports.
  Sunshine
  # AMD VAAPI hardware video encoders (H264/HEVC/AV1), stripped from stock mesa
  # for patent reasons. Without this, Sunshine falls back to software x264 --
  # unusable at 4K. From RPM Fusion free; installs to /usr/lib64/dri-freeworld/
  # (no conflict with base mesa, so a plain layer, not an override).
  mesa-va-drivers-freeworld
)
