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
         docker
         docker-compose
         eza
         fzf
         git
         jj
         macos
         mercurial
         rust
         tmux
         zoxide
        )

# ======================================================
# User configuration

zstyle ':omz:plugins:eza' 'icons' yes
zstyle ':omz:plugins:eza' 'color-scale' all
zstyle ':omz:plugins:eza' 'size-prefix' si

export PATH="/usr/local/sbin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/go/bin:$PATH"
export PATH="$PATH:$HOME/.modular/bin"

export CLICOLOR=1
export GPG_TTY=$(tty)
export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=1048576
export HISTSIZE=1048576
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export TERM=xterm-256color
export VISUAL=nvim
export PYTORCH_ENABLE_MPS_FALLBACK=1
export ELECTRON_OZONE_PLATFORM_HINT=auto
export EDITOR=nvim

alias llt='eza -l --icons --tree -L 3'
alias ltt='eza -l --icons --tree -L 3'
alias -g F='| fzf'
alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python3 -m http.server 8081'
test -e "/opt/homebrew/bin/gdu-go" && alias ncdu='gdu-go'

command -v "mise" >/dev/null && eval "$(mise activate zsh)"
test -e "${HOME}/.local/bin/mise" && eval "$(~/.local/bin/mise activate zsh)"
test -e "/opt/homebrew/bin/brew" && eval "$(/opt/homebrew/bin/brew shellenv)"
test -e "homebrew/bin/brew" && eval "$(homebrew/bin/brew shellenv)"
test -e "${HOME}/.ghcup/env" && . "${HOME}/.ghcup/env"
command -v starship >/dev/null && eval "$(starship init zsh)"
command -v opam >/dev/null && eval "$(opam config env)"

zknew() {
  read "title?Enter note title: "
  pushd $HOME/notes
  zk new --group inbox --title "$title"
  popd
}

nv () {
    if [ -n "$FLOATERM" ]; then
        eval "$FLOATERM" "$@"
    elif [ -e /tmp/nvim.pipe ]; then
        nvim --server /tmp/nvim.pipe --remote "$(realpath $1)"
    else
        nvim --listen /tmp/nvim.pipe "$@"
    fi
}

mvln () {
    fname=`basename "$1"`
    dest=$(echo "$2" | sed 's:/*$::')
    set -x
    mv "$1" "$2"
    ln -s "$dest/$fname" "$1"
    set +x
}

function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

function gethash() {
  if [[ -z "$1" ]]; then
    echo "Usage: get_hash_from_tmux <pattern>" >&2
    return 1
  fi

  local pattern="$1"
  HASH=$(tmux capture-pane -p -S -100 | grep -- "$pattern" | grep -oE '[a-f0-9]{7,40}' | head -1)

  echo "$HASH"
}

## FB
export PATH="$HOME/infer/infer/bin:$HOME/infer/facebook/dependencies/bin:$HOME/devserver/scripts:$PATH"
export BUILD_MODE=default
export MANPATH="$HOME/infer/infer/man":$MANPATH

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
