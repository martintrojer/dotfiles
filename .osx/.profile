export PS1="\u:\w$ "
alias ls="ls -G -F"

export PATH="/usr/local/sbin:$HOME/.local/bin:$PATH"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=1048576
export HISTFILESIZE=1048576
shopt -s histappend
export BASH_SILENCE_DEPRECATION_WARNING=1

export GPG_TTY=$(tty)

export TERM=xterm-256color
export JAVA_HOME=$(/usr/libexec/java_home)

export VISUAL=emacsclient
alias la='ls -lah'
alias serve='python -m SimpleHTTPServer 8081'
alias turtle='stack ghci --package turtle --package text --package lens --package containers --package mtl --package aeson --package lens-aeson --package regex-with-pcre'

function ec
{
    emacsclient -n "$@"
}
function vdiff
{
    emacsclient -c --eval "(vdiff-files \"$1\" \"$2\")"
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

function nukebook
{
    hg book | awk '{print $1}' | xargs hg book -d
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

function dotty
{
    temp_file=$(mktemp /tmp/dotty.XXXX)
    temp_file+=".png"
    dot -Tpng -o "$temp_file" "$1"
    open $temp_file
}
