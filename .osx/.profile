export PS1="\u:\w$ "
alias ls="ls -G -F"

export PATH="/usr/local/sbin:$HOME/.local/bin:$PATH"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export HISTSIZE=1048576
export HISTFILESIZE=1048576
export GPG_TTY=$(tty)

export TERM=xterm-256color
export JAVA_HOME=$(/usr/libexec/java_home)

export VISUAL=nano
alias la='ls -lah'
alias serve='python -m SimpleHTTPServer 8081'
alias turtle='stack ghci --package turtle --package text --package lens --package containers --package mtl --package aeson --package lens-aeson --package regex-pcre'

function ec
{
    emacsclient -n "$@"
}
function mvln
{
    fname=`basename "$1"`
    dest=$(echo "$2" | sed 's:/*$::')
    set -x
    mv "$1" "$2"
    ln -s "$dest/$fname" "$1"
    set +x
}

alias ddev='eval $(docker-machine env dev)'
alias da='docker attach'
alias dps='docker ps --all'
alias dimg='docker images'
alias drmc='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -q --filter "dangling=true")'

eval $(thefuck --alias)

eval `opam config env`

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

## FB
export PATH="/Users/mtrojer/infer/infer/bin:/Users/mtrojer/devserver/scripts:$PATH"
export BUILD_MODE=default
alias fb='cd ~/fbsource/fbobjc'
alias inf='cd ~/infer'
alias imod='cd ~/infer/infer/models'
alias ispec='cd ~/infer/infer/lib/specs'

alias port_forward='ssh -L 8081:localhost:8081 dev'
