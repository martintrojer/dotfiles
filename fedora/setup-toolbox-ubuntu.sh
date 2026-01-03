apt-get update && apt-get install -y \
  locales \
  software-properties-common \
  p7zip-full \
  automake \
  binutils \
  build-essential \
  btop \
  fd-find \
  gdu \
  htop \
  rclone \
  ripgrep \
  stow \
  tmux \
  vim \
  zsh

# Add fastfetch PPA and install
add-apt-repository -y ppa:zhangsongcui3371/fastfetch
apt-get update
apt-get install -y fastfetch

# Generate and set locale
sed -i '/en_US.UTF-8/s/^#//' /etc/locale.gen
locale-gen en_US.UTF-8
update-locale LANG=en_US.UTF-8
export LANG=en_US.UTF-8

