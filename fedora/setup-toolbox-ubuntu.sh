#!/bin/bash
set -euo pipefail

apt-get update && apt-get install -y \
  locales \
  automake \
  binutils \
  build-essential \
  btop \
  fd-find \
  gdu \
  git \
  git-lfs \
  jq \
  ripgrep \
  stow \
  tmux \
  zsh

# Generate and set locale
sed -i '/en_US.UTF-8/s/^#//' /etc/locale.gen
locale-gen en_US.UTF-8
update-locale LANG=en_US.UTF-8
export LANG=en_US.UTF-8
