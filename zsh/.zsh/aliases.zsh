# eza-backed replacements for the standard ls family.
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias lla='eza -la --icons=auto --group-directories-first'
alias la='eza -a --icons=auto --group-directories-first'
alias lt='eza -a --icons=auto --tree -L 3'
alias llt='eza -la --icons=auto --tree -L 3'
# Tree-list depth presets.
alias lt5='eza -a --icons=auto --tree -L 5'
alias llt5='eza -la --icons=auto --tree -L 5'
alias l='eza -lahG --icons=auto --no-permissions --no-user'

# General aliases
alias -g F='| fzf'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g L='| less'
alias -g LL='2>&1 | less'
alias -g NE='2> /dev/null'
alias -g NUL='> /dev/null 2>&1'
alias serve='python3 -m http.server 8081'
alias td='cd "$(trashd)"'
alias gvd='git difftool --dir-diff --no-prompt --extcmd=nvdiff'
alias jvd='jj --no-pager diff --tool nvdiff'

# fd-backed recursive find helpers from the old OMZ setup.
if command -v fd >/dev/null 2>&1; then
  alias ff='fd --hidden --exclude .git --exclude .jj --type f'
  alias ffd='fd --hidden --exclude .git --exclude .jj --type d'
fi

# Common `less` misspelling (the only one with history hits).
alias elss='less'
