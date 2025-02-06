export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="minimal"

zstyle ':omz:update' mode auto      # update automatically without asking
zstyle ':omz:update' frequency 13

# ENABLE_CORRECTION="true"
# COMPLETION_WAITING_DOTS="true"

DISABLE_UNTRACKED_FILES_DIRTY="true"

plugins=(
         brew
         colorize
         common-aliases
         fzf
         git
         iterm2
         macos
         mercurial
         mosh
         rust
         tmux
         zoxide
        )

# ======================================================
# User configuration

export PATH="/usr/local/sbin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/go/bin:$PATH"

export CLICOLOR=1
export GPG_TTY=$(tty)
export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=1048576
export HISTSIZE=1048576
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export TERM=xterm-256color
export VISUAL=nvim

nv () {
    if [ -n "$FLOATERM" ]; then
        eval "$FLOATERM" "$@"
    elif [ -e /tmp/nvim.pipe ]; then
        nvim --server /tmp/nvim.pipe --remote "$(realpath $1)"
    else
        nvim --listen /tmp/nvim.pipe "$@"
    fi
}

alias llt='eza -l --icons --tree -L 3'
alias ltt='eza -l --icons --tree -L 3'

alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python -m SimpleHTTPServer 8081'

mvln () {
    fname=`basename "$1"`
    dest=$(echo "$2" | sed 's:/*$::')
    set -x
    mv "$1" "$2"
    ln -s "$dest/$fname" "$1"
    set +x
}

test -e "/opt/homebrew/bin/gdu-go" && alias ncdu='gdu-go'

test -e "/opt/homebrew/bin/brew" && eval "$(/opt/homebrew/bin/brew shellenv)"
test -e "homebrew/bin/brew" && eval "$(homebrew/bin/brew shellenv)"
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh" || true
command -v starship >/dev/null && eval "$(starship init zsh)"

alias -g F='| fzf'

source $HOME/.api_keys.sh

[ -f "/Users/martintrojer/.ghcup/env" ] && . "/Users/martintrojer/.ghcup/env" # ghcup-env

## FB
export PATH="$HOME/infer/infer/bin:$HOME/infer/facebook/dependencies/bin:$HOME/devserver/scripts:$HOME/devenv/bin:$PATH"

command -v opam >/dev/null && eval "$(opam config env)"

export BUILD_MODE=default
export LD_LIBRARY_PATH="$HOME/devenv/lib"
export MANPATH="$HOME/infer/infer/man":$MANPATH
export PKG_CONFIG_PATH="$HOME/devenv/lib/pkgconfig"

proxy () {
    export https_proxy=fwdproxy:8080
    export http_proxy=fwdproxy:8080
}
unproxy () {
    unset https_proxy
    unset http_proxy
}

# ======================================================

source $ZSH/oh-my-zsh.sh
