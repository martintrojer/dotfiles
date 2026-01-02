dnf copr enable -y lihaohong/yazi
dnf copr enable -y terjeros/eza
dnf copr enable -y atim/starship

dnf install -y \
  vim neovim \
  zsh \
  htop btop \
  gdu fd-find ripgrep yazi \
  fzf zoxide tmux bat eza starship \
  nodejs 
