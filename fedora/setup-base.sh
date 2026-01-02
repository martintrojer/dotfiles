# https://copr.fedorainfracloud.org/coprs/lihaohong/yazi/
# https://copr.fedorainfracloud.org/coprs/terjeros/eza
# https://copr.fedorainfracloud.org/coprs/atim/starship

# Download the .repo file. there is a download link for YOUR release
# Copy to /etc/yum.repos.d:

# rpm-ostree refresh-md

rpm-ostree install \
  vim neovim \
  zsh \
  htop btop grubby \
  stow rclone gdu fd-find ripgrep 7zip yazi \
  fzf zoxide tmux bat eza starship \
  distrobox
