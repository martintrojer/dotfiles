# PATH configuration
export PATH="/usr/local/sbin:$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$HOME/go/bin:$PATH"
export PATH="$PATH:$HOME/.opencode/bin:$HOME/.pixi/bin:$HOME/.modular/bin"

# Terminal and display
# Let the terminal emulator set TERM so tmux can detect real capabilities.
# Fall back to xterm-256color when the host terminfo is missing (e.g. SSH).
if [[ -z "$TMUX" ]] && ! infocmp "$TERM" &>/dev/null 2>&1; then
  export TERM=xterm-256color
fi
export CLICOLOR=1

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
# Prefer a user-installed lesspipe explicitly so system profile defaults
# do not win when PATH is still in flux during shell startup.
lesspipe_path=""
if [ -x "$HOME/.local/bin/lesspipe.sh" ]; then
  lesspipe_path="$HOME/.local/bin/lesspipe.sh"
elif command -v lesspipe.sh >/dev/null 2>&1; then
  lesspipe_path=$(command -v lesspipe.sh)
elif [ -x "/usr/bin/lesspipe.sh" ]; then
  lesspipe_path="/usr/bin/lesspipe.sh"
fi

if [ -n "$lesspipe_path" ]; then
  export LESSOPEN="| $lesspipe_path %s"
  export LESS="-R"
fi

if [ -n "$lesspipe_path" ] && [ -n "$bat_cmd" ]; then
  export LESSCOLORIZER="$bat_cmd"
fi
