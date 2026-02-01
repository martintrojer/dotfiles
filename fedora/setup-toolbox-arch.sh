#!/bin/bash
set -euo pipefail

pacman -S --noconfirm \
  p7zip \
  automake \
  base-devel \
  btop \
  fastfetch \
  fd \
  gcc \
  gdu \
  git \
  git-lfs \
  glibc \
  make \
  rclone \
  ripgrep \
  stow \
  tmux \
  vim \
  wtype \
  zsh

# Generate and set locale
sed -i '/en_US.UTF-8/s/^#//' /etc/locale.gen
locale-gen
echo 'LANG=en_US.UTF-8' > /etc/locale.conf
export LANG=en_US.UTF-8

# Install yay (AUR helper)
if ! command -v yay &> /dev/null; then
  # Find a non-root user to run makepkg (required, as it cannot run as root)
  USER=$(getent passwd | awk -F: '$3 >= 1000 && $1 != "nobody" {print $1; exit}')
  if [ -z "$USER" ]; then
    echo "ERROR: No non-root user found. yay installation requires a non-root user." >&2
    exit 1
  fi

  cd /tmp
  git clone https://aur.archlinux.org/yay.git
  chown -R "$USER:$USER" /tmp/yay
  cd yay
  sudo -u "$USER" makepkg -si --noconfirm
  cd /
  rm -rf /tmp/yay
fi
