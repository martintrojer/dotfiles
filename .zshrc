# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="minimal"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
# zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
# zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
         branch
         brew
         colorize
         common-aliases
         emacs
         git
         iterm2
         macos
         mercurial
         mosh
         ripgrep
         rust
         tmux
         zsh-navigation-tools
        )

# ======================================================
# User configuration

autoload znt-history-widget
zle -N znt-history-widget
bindkey "^R" znt-history-widget

export PATH="/usr/local/sbin:$HOME/.local/bin:$PATH"
export PATH="/opt/homebrew/opt/ruby@3.1/bin:$HOME/.local/share/gem/ruby/3.1.0/bin:$PATH"

export CLICOLOR=1
export GPG_TTY=$(tty)
export HISTCONTROL=ignoredups:erasedups
export HISTFILESIZE=1048576
export HISTSIZE=1048576
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export TERM=xterm-256color
export VISUAL=nvim

alias port_forward='ssh -L 8081:localhost:8081 dev'
alias serve='python -m SimpleHTTPServer 8081'

vdiff () {
    emacsclient -c --eval "(vdiff-files \"$1\" \"$2\")"
}

ediff () {
    emacsclient -c --eval "(progn ((ediff-files \"$1\" \"$2\")))"
}

mvln () {
    fname=`basename "$1"`
    dest=$(echo "$2" | sed 's:/*$::')
    set -x
    mv "$1" "$2"
    ln -s "$dest/$fname" "$1"
    set +x
}

if [ -x "$(which opam)" ]; then
    eval `opam config env`
fi

test -e "/opt/homebrew/bin/brew" && eval "$(/opt/homebrew/bin/brew shellenv)"
test -e "homebrew/bin/brew" && eval "$(homebrew/bin/brew shellenv)"
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

## FB
export PATH="$HOME/infer/infer/bin:$HOME/infer/facebook/dependencies/bin:$HOME/devserver/scripts:$HOME/devenv/bin:$PATH"

export BUILD_MODE=default
export LD_LIBRARY_PATH=/home/mtrojer/devenv/lib
export MANPATH="$HOME/infer/infer/man":$MANPATH
export PKG_CONFIG_PATH=/home/mtrojer/devenv/lib/pkgconfig

proxy () {
    export https_proxy=fwdproxy:8080
    export http_proxy=fwdproxy:8080
}
unproxy () {
    unset https_proxy
    unset http_proxy
}
nukebook () {
    hg book | awk '{print $1}' | xargs hg book -d
}

HOTLIST_FILE=~/.ncd_hotlist.txt

znt_cd_hotlist=("${(f)$(<$HOTLIST_FILE)}")

ncd_add () {
    echo "$PWD" >> $HOTLIST_FILE
    sort -o $HOTLIST_FILE $HOTLIST_FILE
    znt_cd_hotlist=("${(f)$(<$HOTLIST_FILE)}")
}

ncd_rm () {
    sed -i "/$1/d" $HOTLIST_FILE
    znt_cd_hotlist=("${(f)$(<$HOTLIST_FILE)}")
}

# ======================================================

source $ZSH/oh-my-zsh.sh
