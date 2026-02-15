# General aliases
alias -g F='| fzf'
alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python3 -m http.server 8081'
test -e "/opt/homebrew/bin/gdu-go" && alias gdu='gdu-go'
