export ZSH="$HOME/.oh-my-zsh"

# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="minimal"

zstyle ':omz:update' mode auto      # update automatically without asking
zstyle ':omz:update' frequency 13

# ENABLE_CORRECTION="true"
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

bindkey '^E' autosuggest-accept

zstyle ':omz:plugins:eza' 'icons' yes
zstyle ':omz:plugins:eza' 'color-scale' all
zstyle ':omz:plugins:eza' 'size-prefix' si

zstyle ':omz:plugins:alias-finder' autoload yes
zstyle ':omz:plugins:alias-finder' cheaper yes

# ======================================================
# Exports
# ======================================================

# PATH configuration
export PATH="/usr/local/sbin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/go/bin:$PATH"

if [ -d "$HOME/.pixi/bin" ]; then
  export PATH="$PATH:$HOME/.pixi/bin"
fi
if [ -d "$HOME/.opencode/bin" ]; then
  export PATH="$PATH:$HOME/.opencode/bin"
fi
if [ -d "$HOME/.modular/bin" ]; then
  export PATH="$PATH:$HOME/.modular/bin"
fi

# Terminal and display
[[ -z "$TMUX" ]] && export TERM=xterm-256color
export CLICOLOR=1
[[ "$OSTYPE" == "darwin"* ]] && export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

# Editor configuration
export EDITOR=nvim
export VISUAL=nvim

# History configuration
export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=1048576
export HISTSIZE=1048576

# GPG configuration
export GPG_TTY=$(tty)

# Application-specific exports
export ELECTRON_OZONE_PLATFORM_HINT=auto
[[ "$OSTYPE" == "darwin"* ]] && export PYTORCH_ENABLE_MPS_FALLBACK=1

# ======================================================
# Tool initialization
# ======================================================

# Initialize mise (version manager)
if command -v mise >/dev/null; then
  eval "$(mise activate zsh)"
elif test -e "${HOME}/.local/bin/mise"; then
  eval "$(~/.local/bin/mise activate zsh)"
fi

# Initialize Homebrew (macOS only)
if [[ "$OSTYPE" == "darwin"* ]]; then
  if test -e "/opt/homebrew/bin/brew"; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
  elif test -e "/usr/local/bin/brew"; then
    eval "$(/usr/local/bin/brew shellenv)"
  fi
fi

# Initialize other tools
test -e "${HOME}/.ghcup/env" && . "${HOME}/.ghcup/env"
command -v starship >/dev/null && eval "$(starship init zsh)"
command -v opam >/dev/null && eval "$(opam config env)"

# ======================================================
# Exports (PATH-dependent)
# ======================================================

# PAGER and LESS configuration (requires PATH to be fully configured)
bat_cmd=""
if command -v bat >/dev/null 2>&1; then
  bat_cmd="bat"
elif command -v batcat >/dev/null 2>&1; then
  bat_cmd="batcat"
fi

if [ -n "$bat_cmd" ]; then
  export PAGER="$bat_cmd"
  export MANPAGER="sh -c 'col -bx | $bat_cmd -l man -p'"
  export MANROFFOPT="-c"
fi

# LESS configuration with lesspipe and bat/batcat
lesspipe_path=""
if command -v lesspipe.sh >/dev/null 2>&1; then
  lesspipe_path=$(command -v lesspipe.sh)
elif [ -f "/usr/bin/lesspipe.sh" ]; then
  lesspipe_path="/usr/bin/lesspipe.sh"
elif [[ "$OSTYPE" == "darwin"* ]] && [ -f "/opt/homebrew/bin/lesspipe.sh" ]; then
  lesspipe_path="/opt/homebrew/bin/lesspipe.sh"
fi

if [ -n "$lesspipe_path" ] && [ -n "$bat_cmd" ]; then
  export LESSOPEN="| $lesspipe_path %s"
  export LESS="-R"
  export LESSCOLORIZER="$bat_cmd"
fi

# ======================================================
# Aliases
# ======================================================

# General aliases
alias -g F='| fzf'
alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python3 -m http.server 8081'
test -e "/opt/homebrew/bin/gdu-go" && alias gdu='gdu-go'

# Note: eza aliases (ls, ll, la, etc.) are defined after OMZ source
# to override OMZ defaults - see "Aliases (override OMZ defaults)" section below

# ======================================================
# Functions
# ======================================================

# zknew: Prompt for a note title and create a new note in the inbox group using zk in $HOME/notes.
zknew() {
  local title
  read "title?Enter note title: "
  (
    cd "$HOME/notes" || return 1
    zk new --group inbox --title "$title"
  )
}

# nv: Open or connect to Neovim in a shared session.
# - If /tmp/nvim.pipe exists, open file via --remote; otherwise, start Neovim with server.
nv() {
  if [[ -e /tmp/nvim.pipe ]]; then
    nvim --server /tmp/nvim.pipe --remote "$(realpath "$1")"
  else
    nvim --listen /tmp/nvim.pipe "$@"
  fi
}

# mvln: move a file and create a symlink at its old path pointing to the new location
mvln() {
  if [[ $# -ne 2 ]]; then
    echo "Usage: mvln <source> <destination>"
    return 1
  fi
  set -x
  mv -iv "$1" "$2" && ln -s "$(realpath "$2")" "$1"
  set +x
}

# y: Open Yazi file manager in the current directory, exit with the selected directory.
function y() {
	local tmp="$(mktemp -t "yazi-cwd.XXXXXX")" cwd
	yazi "$@" --cwd-file="$tmp"
	if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
		builtin cd -- "$cwd"
	fi
	rm -f -- "$tmp"
}

# gh: Extract the first git hash matching a pattern from the last 100 lines of the current tmux pane.
# Usage: gh <pattern>
ghash() {
  if [[ -z "$TMUX" ]]; then
    echo "Error: Not in a tmux session" >&2
    return 1
  fi
  if [[ $# -ne 1 ]]; then
    echo "Usage: ghash <pattern>" >&2
    return 1
  fi
  local pattern="$1"
  local hash
  hash=$(tmux capture-pane -p -S -100 | grep -- "$pattern" | grep -oE '[a-f0-9]{7,40}' | head -n1)
  if [[ -n "$hash" ]]; then
    echo "$hash"
    # Use tmux clipboard integration (works locally and over SSH via OSC 52)
    # This requires 'set-clipboard on' in tmux.conf
    # The -w flag writes to the system clipboard
    tmux set-buffer -w "$hash"
  else
    echo "No hash found matching pattern: $pattern"
    return 1
  fi
}

# hgup: Look up a git hash by pattern and update hg to it
hgup() {
  local hash
  hash=$(ghash "$1") || return 1
  hg up "$hash"
}

# Remove all history lines matching a pattern
rmhist() {
  if (( $# != 1 )); then
    echo "Usage: rmhist <pattern>"
    return 1
  fi
  local histfile=${HISTFILE:-$HOME/.zsh_history}
  local tmpfile
  tmpfile=$(mktemp) || return
  grep -v -- "$1" "$histfile" > "$tmpfile" && mv -- "$tmpfile" "$histfile"
  fc -R "$histfile"
  echo "Removed history lines matching: $1"
}

# ======================================================
## FB-specific configuration
# ======================================================

if [[ -d "$HOME/infer" ]] || [[ -d "$HOME/devserver" ]]; then
  # FB-specific PATH and environment
  export PATH="$HOME/infer/infer/bin:$HOME/infer/facebook/dependencies/bin:$HOME/devserver/scripts:$PATH"
  export BUILD_MODE=default
  export MANPATH="$HOME/infer/infer/man":$MANPATH

  proxy() {
    local p=fwdproxy:8080
    HTTPS_PROXY=$p HTTP_PROXY=$p https_proxy=$p http_proxy=$p "$@"
  }

  proxy_py() {
    local p=http://fwdproxy:8080
    HTTPS_PROXY=$p HTTP_PROXY=$p https_proxy=$p http_proxy=$p "$@"
  }

fi

# ======================================================

source $ZSH/oh-my-zsh.sh

# ======================================================
# Aliases (override OMZ defaults)
# ======================================================

# eza-compliant versions of standard ls aliases (plus some extras)
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias la='eza -a --icons=auto'
alias lt='eza --tree' # Replaces standard time-sort alias with eza's tree view
alias llt='eza -l --icons --tree -L 3'
alias ltt='eza -l --icons --tree -L 3'
alias l='eza -lahG --icons=auto'

if [[ -f /run/.toolboxenv ]]; then
    echo "You are inside a Toolbox container."
    export TERM=xterm-256color
    PROMPT="%F{magenta}[⬢] %f$PROMPT"
fi
