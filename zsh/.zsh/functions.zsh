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

# `m` (markdown reader) lives at local-bin/.local/bin/m. It used to be a
# zsh function with an inline python pty fork; promoted to a real script
# so the workaround is documented properly. See that file's docstring.

# tm: tmux session picker (pinned sessions + live sessions + zoxide + finder).
tm() {
  "$HOME/.config/tmux/scripts/tms" pick-and-connect
}

