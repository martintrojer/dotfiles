# eza-compliant versions of standard ls aliases (plus some extras)
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias la='eza -a --icons=auto'
alias lt='eza --tree'
# Tree-list 3 levels deep, including .dotfiles.
alias ltt='eza -la --icons --tree -L 3'
alias l='eza -lahG --icons=auto --no-permissions --no-user'

if [[ -f /run/.toolboxenv ]]; then
    export TERM=xterm-256color
fi

tm() {
    "$HOME/.config/tmux/scripts/tms" pick-and-connect
}
