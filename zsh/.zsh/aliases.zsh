# General aliases
alias -g F='| fzf'
alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python3 -m http.server 8081'
test -e "/opt/homebrew/bin/gdu-go" && alias gdu='gdu-go'

# Common `less` misspellings
alias lees='less'
alias elss='less'
alias sless='less'

# Only alias ollama to toolbox if it's not installed on the host
if ! command -v ollama >/dev/null 2>&1; then
    alias ollama='toolbox run -c ollama ollama'
fi
