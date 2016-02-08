export PS1="\u:\w$ "
alias ls="ls -G -F"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

export VISUAL="emacs -Q -nw"
export TERM=xterm-256color
export JAVA_HOME=$(/usr/libexec/java_home)

alias git=hub

alias nano=$VISUAL
alias vim=$VISUAL
alias vi=$VISUAL

alias ddev='eval $(docker-machine env dev)'
alias dosio='eval $(docker-machine env osio)'
alias osio='export OSIO_LOCAL=`docker-machine ip osio`'
alias da='docker attach'
alias dps='docker ps --all'
alias dimg='docker images'
alias drmc='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -q --filter "dangling=true")'

eval $(thefuck --alias)
