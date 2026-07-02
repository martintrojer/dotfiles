#!/bin/bash

base_packages=(
  # Provides linker and binary utilities for native builds.
  binutils
  # C toolchain needed for native builds.
  gcc
  # C++ toolchain needed for native builds.
  gcc-c++
  # Required for git-based workflows.
  git
  # Needed for repositories that store large assets in Git LFS.
  git-lfs
  # Build driver used by source installs and Make-based projects.
  make
  # Required to apply these dotfiles with GNU Stow.
  stow
  # Terminal multiplexer with repo-managed config.
  tmux
  # Preferred interactive shell.
  zsh
)
