# sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
# git clone --depth 1 https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
# git clone --depth 1 https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting

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

export PATH="/usr/local/sbin:$HOME/.local/bin:$HOME/.cargo/bin:$HOME/go/bin:$PATH"
export PATH="$PATH:$HOME/.modular/bin"

export CLICOLOR=1
export GPG_TTY=$(tty)
export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=1048576
export HISTSIZE=1048576
# LSCOLORS is macOS-specific, removed for Linux compatibility
[[ "$OSTYPE" == "darwin"* ]] && export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export TERM=xterm-256color
export VISUAL=nvim
# PYTORCH_ENABLE_MPS_FALLBACK is macOS-only (Metal Performance Shaders)
[[ "$OSTYPE" == "darwin"* ]] && export PYTORCH_ENABLE_MPS_FALLBACK=1
export ELECTRON_OZONE_PLATFORM_HINT=auto
export EDITOR=nvim

alias -g F='| fzf'
alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python3 -m http.server 8081'
test -e "/opt/homebrew/bin/gdu-go" && alias ncdu='gdu-go'

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

# remove from history all lines matching a pattern
function hist-rm() {
  if [[ -z "$1" ]]; then
    echo "Usage: hist-rm <pattern>"
    return 1
  fi

  local tmpfile
  tmpfile=$(mktemp)

  # Ensure HISTFILE is set
  local histfile="${HISTFILE:-$HOME/.zsh_history}"

  # Filter out lines matching the pattern
  grep -v -- "$1" "$histfile" > "$tmpfile" && mv "$tmpfile" "$histfile"

  # Reload history into current shell
  fc -R "$histfile"

  echo "Removed history lines matching: $1"
}

## FB-specific configuration (only load if directories exist)
if [[ -d "$HOME/infer" ]] || [[ -d "$HOME/devserver" ]]; then
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
fi

# ======================================================

source $ZSH/oh-my-zsh.sh

# eza-compliant versions of stansard ls alises (plus some extras)
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias la='eza -a --icons=auto'
alias lt='eza --tree' # Replaces standard time-sort alias with eza's tree view
alias llt='eza -l --icons --tree -L 3'
alias ltt='eza -l --icons --tree -L 3'
alias l='eza -lahG --icons=auto'
