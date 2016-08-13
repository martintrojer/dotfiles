export PS1="\u:\w$ "
alias ls="ls -G -F"

export PATH="/usr/local/sbin:$PATH"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export HISTSIZE=1048576
export HISTFILESIZE=1048576

export VISUAL="emacs -Q -nw"
export TERM=xterm-256color
export JAVA_HOME=$(/usr/libexec/java_home)

alias nano=$VISUAL
alias vim=$VISUAL
alias vi=$VISUAL

alias ddev='eval $(docker-machine env dev)'
alias da='docker attach'
alias dps='docker ps --all'
alias dimg='docker images'
alias drmc='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -q --filter "dangling=true")'

eval $(thefuck --alias)

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
