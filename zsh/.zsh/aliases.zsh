# General aliases
alias -g F='| fzf'
alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python3 -m http.server 8081'
test -e "/opt/homebrew/bin/gdu-go" && alias gdu='gdu-go'

# Common `less` misspellings
alias lees='less'
alias elss='less'
alias sless='less'

# Only alias ollama to toolbox on Fedora-family distros if it's not installed on the host
if [[ -r /etc/os-release ]]; then
    # shellcheck disable=SC1091
    . /etc/os-release

    distro_id_lc="${ID:l}"
    distro_like_lc="${ID_LIKE:l}"
    if [[ "$distro_id_lc" == *fedora* || "$distro_like_lc" == *fedora* || "$distro_like_lc" == *rhel* ]] \
        && ! command -v ollama >/dev/null 2>&1; then
        alias ollama='toolbox run -c ollama ollama'
    fi
fi
