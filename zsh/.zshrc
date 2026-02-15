export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="minimal"

zstyle ':omz:update' mode auto
zstyle ':omz:update' frequency 13

# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

# Base plugins (OS-agnostic)
plugins=(
         alias-finder
         colorize
         common-aliases
         docker
         docker-compose
         eza
         fzf
         git
         jj
         mercurial
         rust
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

bindkey '^E' autosuggest-accept

zstyle ':omz:plugins:eza' 'icons' yes
zstyle ':omz:plugins:eza' 'color-scale' all
zstyle ':omz:plugins:eza' 'size-prefix' si

zstyle ':omz:plugins:alias-finder' autoload yes
zstyle ':omz:plugins:alias-finder' cheaper yes

# ======================================================
# Modular config (sourced before omz)
# ======================================================

for file in $HOME/.zsh/*.zsh; do
  source "$file"
done

# ======================================================

source $ZSH/oh-my-zsh.sh

# ======================================================
# Aliases (override OMZ defaults)
# ======================================================

# Source overrides after omz
source $HOME/.zsh/overrides.zsh
