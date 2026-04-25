export ZSH="$HOME/.oh-my-zsh"

# Keep inherited completion paths from compounding across nested shells.
typeset +x FPATH
typeset -U path fpath

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="minimal"

zstyle ':omz:update' mode reminder
zstyle ':omz:update' frequency 30

# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"
ZSH_DISABLE_COMPFIX=true

# Base plugins (OS-agnostic)
plugins=(
         alias-finder
         eza
         fzf
         git
         jj
         podman
         rust
         ssh
         sudo
         systemd
         toolbox
         tmux
         zsh-autosuggestions
         zsh-syntax-highlighting
         zoxide
        )

# macOS-specific plugins
if [[ "$OSTYPE" == "darwin"* ]]; then
  plugins+=(brew macos)
fi

# ======================================================
# User configuration
# ======================================================

bindkey '^E' end-of-line
bindkey '^[[C' autosuggest-accept

zstyle ':omz:plugins:eza' 'icons' yes
zstyle ':omz:plugins:eza' 'color-scale' all
zstyle ':omz:plugins:eza' 'size-prefix' si

zstyle ':omz:plugins:alias-finder' autoload yes
zstyle ':omz:plugins:alias-finder' cheaper yes

# ======================================================
# Modular config (sourced before omz)
# ======================================================

# Homebrew-backed tools must be on PATH before OMZ plugins load.
[[ "$OSTYPE" == darwin* ]] && [[ -f "$HOME/.zsh/homebrew.zsh" ]] && source "$HOME/.zsh/homebrew.zsh"

if [[ "$OSTYPE" == darwin* ]] && [[ -f "$HOME/.zsh/os-darwin.zsh" ]]; then
  source "$HOME/.zsh/os-darwin.zsh"
fi

for file in $HOME/.zsh/*.zsh; do
  case "${file:t}" in
    homebrew.zsh|os-darwin.zsh)
      continue
      ;;
  esac
  source "$file"
done

# ======================================================

source $ZSH/oh-my-zsh.sh

# ======================================================
# Aliases (override OMZ defaults)
# ======================================================

# Source overrides after omz
source $HOME/.zsh/overrides.zsh

# Don't leak OMZ/plugin completion paths into child shell environments.
typeset +x FPATH
