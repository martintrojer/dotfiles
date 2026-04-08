#!/bin/bash

base_packages=(
  # Needed when building some tools from source.
  automake
  # Provides linker and binary utilities for native builds.
  binutils
  # Preferred system monitor.
  btop
  # Fast file finder used throughout the shell workflow.
  fd-find
  # C toolchain needed for native builds.
  gcc
  # C++ toolchain needed for native builds.
  gcc-c++
  # Disk usage inspection tool.
  gdu
  # Required for git-based workflows.
  git
  # Needed for repositories that store large assets in Git LFS.
  git-lfs
  # Build driver used by source installs and Make-based projects.
  make
  # Fast text search used heavily across the repo.
  ripgrep
  # Required to apply these dotfiles with GNU Stow.
  stow
  # Terminal multiplexer with repo-managed config.
  tmux
  # Preferred interactive shell.
  zsh
)

workstation_extra_packages=()
