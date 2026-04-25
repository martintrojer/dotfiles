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
