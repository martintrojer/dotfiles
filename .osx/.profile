export PS1="\u:\w$ "
alias ls="ls -G -F"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx

export VISUAL="emacs -Q -nw"
export TERM=xterm-256color
export JAVA_HOME=$(/usr/libexec/java_home)

alias nano=$VISUAL
alias vim=$VISUAL
alias vi=$VISUAL

alias dinit='eval $(docker-machine env docker-dev)'
alias dps='docker ps --all'
alias dimg='docker images'
alias drmc='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -q --filter "dangling=true")'

alias devbox='ssh localhost -p 2222'

#xmodmap ~/.xmodmap 2>/dev/null

