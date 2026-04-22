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

# m: Render a markdown file with glow, sized to the current terminal width
m() {
  glow -p -w "$(($(tput cols) - 6))" "$@"
}

# openrouter: Run a command with OPENROUTER_API from OpenClaw auth profiles.
openrouter() {
  if (( $# == 0 )); then
    echo "Usage: openrouter <command> [args...]" >&2
    return 1
  fi

  local auth_file="$HOME/.openclaw/agents/main/agent/auth-profiles.json"
  if [[ ! -r "$auth_file" ]]; then
    echo "Error: Cannot read auth file: $auth_file" >&2
    return 1
  fi

  local key
  if command -v jq >/dev/null 2>&1; then
    key=$(jq -r '
      .lastGood.openrouter as $profile
      | if ($profile != null and (.profiles[$profile].key // "") != "")
        then .profiles[$profile].key
        else (
          .profiles
          | to_entries
          | map(select(.value.provider == "openrouter" and (.value.key // "") != ""))
          | .[0].value.key // ""
        )
        end
    ' "$auth_file")
  else
    key=$(sed -nE 's/.*"key"[[:space:]]*:[[:space:]]*"([^"]+)".*/\1/p' "$auth_file" | head -n1)
  fi

  if [[ -z "$key" ]]; then
    echo "Error: Could not find OpenRouter key in $auth_file" >&2
    return 1
  fi

  OPENROUTER_API="$key" OPENROUTER_API_KEY="$key" "$@"
}
