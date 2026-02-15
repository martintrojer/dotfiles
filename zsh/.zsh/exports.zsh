# PATH configuration
export PATH="/usr/local/sbin:$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$HOME/go/bin:$PATH"
export PATH="$PATH:$HOME/.opencode/bin:$HOME/.pixi/bin:$HOME/.modular/bin"

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
