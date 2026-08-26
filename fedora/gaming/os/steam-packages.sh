#!/bin/bash

# Gaming/Steam packages layered on top of Fedora Sway Atomic (Sericea).
#
# These require an extra repo that is NOT part of the COPR-free host baseline,
# so it must be enabled before running os/setup-steam.sh:
#
# RPM Fusion (free + nonfree) for steam, gamescope, mangohud, gamemode:
#      rpm-ostree install \
#        https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-$(rpm -E %fedora).noarch.rpm \
#        https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-$(rpm -E %fedora).noarch.rpm
#
# Not listed, because the Sericea base image already ships them: gamemode
# (used by gamemoderun/optirun) and 7zip (used by optiscaler-sync). Listing a
# base-image package makes `rpm-ostree install` fail with "already provided
# by" and layer NOTHING, taking the whole array down. If a future rebase drops
# either one, add it back here together with --allow-inactive in
# setup-steam.sh. Check with: rpm-ostree db list "$(rpm-ostree status --json |
# jq -r '.deployments[]|select(.booted)."base-checksum"')" | grep -E '^ (gamemode|7zip)-'
# shellcheck disable=SC2034  # consumed by the setup-*.sh script that sources this.
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
  # AMD VAAPI hardware video encoders (H264/HEVC/AV1), stripped from stock mesa
  # for patent reasons. Without these, Steam Remote Play falls back to software
  # x264. From RPM Fusion free; installs to /usr/lib{,64}/dri-freeworld/ (no
  # conflict with base mesa, so a plain layer, not an override).
  #
  # Both arches are required. Steam's host encoder looks up VAAPI drivers in the
  # 32-bit /usr/lib/dri-freeworld/; with x86_64 alone that dir is empty, libva
  # falls through to the patent-stripped mesa-dri-drivers.i686, and the encoder
  # dies with "Function not implemented". Verify in
  # ~/.local/share/Steam/logs/streaming_log.txt: `>>> Capture method set to`
  # must name VAAPI, not libx264.
  #
  # Both names are arch-qualified because a bare `mesa-va-drivers-freeworld`
  # plus the `.i686` makes rpm-ostree treat the pair as one package and replace
  # the x86_64 with the i686, silently losing 64-bit VAAPI. It reports that as
  # "Downgraded: 26.1.8-1.fc44 -> 26.1.8-1.fc44", which is an arch swap, not a
  # version change. After layering, expect both arches in:
  #   rpm-ostree db list <commit> | grep freeworld
  # Keep them at the same version; a skew is a known way to break mesa on
  # rpm-ostree (rpm-ostree#4592).
  mesa-va-drivers-freeworld.x86_64
  mesa-va-drivers-freeworld.i686
)
