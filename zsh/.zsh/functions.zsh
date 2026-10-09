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

# tm: session picker (pinned sessions + live sessions + zoxide + finder).
# tsesh comes from the mu-crew/tmux-session-picker TPM plugin.
tm() {
  "$HOME/.tmux/plugins/tmux-session-picker/bin/tsesh" pick-and-connect ${1:+--query} ${1:+"$1"}
}

