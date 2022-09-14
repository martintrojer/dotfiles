# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

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
ENABLE_CORRECTION="true"

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
plugins=(brew
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
        )

# ======================================================
# User configuration

export PATH="/usr/local/sbin:$HOME/.local/bin:$PATH"
eval "$(homebrew/bin/brew shellenv)"

export CLICOLOR=1
export LSCOLORS=gxBxhxDxfxhxhxhxhxcxcx
export HISTCONTROL=ignoredups:erasedups
export HISTSIZE=1048576
export HISTFILESIZE=1048576

export GPG_TTY=$(tty)

export TERM=xterm-256color

export VISUAL=emacsclient
alias serve='python -m SimpleHTTPServer 8081'

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

eval `opam config env`

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

alias port_forward='ssh -L 8081:localhost:8081 dev'

## FB
export PATH="/Users/mtrojer/infer/infer/bin:/Users/mtrojer/infer/facebook/dependencies/bin:/Users/mtrojer/devserver/scripts:$PATH"
export BUILD_MODE=default
export PATH="/home/mtrojer/devenv/bin:$PATH"
export PKG_CONFIG_PATH=/home/mtrojer/devenv/lib/pkgconfig
export LD_LIBRARY_PATH=/home/mtrojer/devenv/lib
export MANPATH="/home/mtrojer/infer/infer/man":$MANPATH

function nukebook
{
    hg book | awk '{print $1}' | xargs hg book -d
}
function proxy
{
    export https_proxy=fwdproxy:8080
    export http_proxy=fwdproxy:8080
}
function unproxy
{
    unset https_proxy
    unset http_proxy
}

# ======================================================

source $ZSH/oh-my-zsh.sh
