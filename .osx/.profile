export PS1="\u:\w$ "
alias ls="ls -G -F"
export LS_COLORS="ow=01;90:di=01;90"

export VISUAL="emacs -Q -nw"
export TERM=xterm-256color
export JAVA_HOME=$(/usr/libexec/java_home)

alias nano=$VISUAL
alias vim=$VISUAL
alias vi=$VISUAL

alias devbox='ssh localhost -p 2222'

#xmodmap ~/.xmodmap 2>/dev/null

export PATH=$PATH:/Users/martin/.gem/ruby/2.0.0/bin

