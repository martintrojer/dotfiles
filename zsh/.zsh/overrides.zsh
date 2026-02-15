# eza-compliant versions of standard ls aliases (plus some extras)
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias la='eza -a --icons=auto'
alias lt='eza --tree'
alias llt='eza -l --icons --tree -L 3'
alias ltt='eza -l --icons --tree -L 3'
alias l='eza -lahG --icons=auto'

if [[ -f /run/.toolboxenv ]]; then
    echo "You are inside a Toolbox container."
    export TERM=xterm-256color
    PROMPT="%F{magenta}[⬢] %f$PROMPT"
fi
