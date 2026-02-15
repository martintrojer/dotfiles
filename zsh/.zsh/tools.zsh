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
