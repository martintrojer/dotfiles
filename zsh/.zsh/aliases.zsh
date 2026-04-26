# eza-backed replacements for the standard ls family.
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias la='eza -a --icons=auto'
alias lt='eza --tree'
# Tree-list 3 levels deep, including dotfiles.
alias ltt='eza -la --icons --tree -L 3'
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
alias gvd='git difftool --dir-diff --no-prompt --extcmd=nvdiff'
alias jvd='jj --no-pager diff --tool nvdiff'

# Common `less` misspelling (the only one with history hits).
alias elss='less'
