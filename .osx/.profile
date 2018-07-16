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
function ec
{
emacsclient "$@" &
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
