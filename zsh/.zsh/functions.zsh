# y: Open Yazi; on quit (q), cd the shell to the last real directory.
# Matches upstream shell wrapper: cwd-file is NUL-terminated, and quitting from
# an fd/rg search or VFS writes a URL (fd://…, sftp://…) that is not a local
# directory — [ -d ] skips those so we don't cd into a search view or error.
function y() {
	local tmp cwd
	tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
	command yazi "$@" --cwd-file="$tmp"
	IFS= read -r -d '' cwd < "$tmp"
	[ "$cwd" != "$PWD" ] && [ -d "$cwd" ] && builtin cd -- "$cwd" || builtin true
	command rm -f -- "$tmp"
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

