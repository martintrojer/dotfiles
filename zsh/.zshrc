# Slim zshrc, no framework. Pillar #2 (builtins first), #3 (every line
# understood), #4 (each piece earns its place). The only third-party
# code is zsh-autosuggestions and zsh-syntax-highlighting, cloned into
# ~/.zsh/plugins/<name>/ at pinned versions by stow-all.py --apply.
#
# Replaces oh-my-zsh + 15 plugins; see TODO.md item #1 for the audit.

# ----------------------------------------------------------------------
# Path / fpath hygiene
# ----------------------------------------------------------------------
typeset -U path fpath

# Plugin completion dirs need to be on fpath BEFORE compinit runs.
# ZSH_PLUGINS_DIR mirrors stow-all.py's ZSH_PLUGINS_DEST.
ZSH_PLUGINS_DIR="$HOME/.local/share/zsh-plugins"
fpath=(
  $ZSH_PLUGINS_DIR/zsh-autosuggestions
  $ZSH_PLUGINS_DIR/zsh-syntax-highlighting
  $fpath
)

# ----------------------------------------------------------------------
# OS-specific bootstrap (must precede compinit so PATH is final)
# ----------------------------------------------------------------------
[[ "$OSTYPE" == darwin* ]] && [[ -f "$HOME/.zsh/homebrew.zsh" ]] && source "$HOME/.zsh/homebrew.zsh"
[[ "$OSTYPE" == darwin* ]] && [[ -f "$HOME/.zsh/os-darwin.zsh" ]] && source "$HOME/.zsh/os-darwin.zsh"

# ----------------------------------------------------------------------
# History
# ----------------------------------------------------------------------
HISTFILE="$HOME/.zsh_history"
HISTSIZE=1048576
SAVEHIST=1048576
setopt extended_history hist_expire_dups_first hist_ignore_dups \
       hist_ignore_space hist_verify share_history

# ----------------------------------------------------------------------
# Directory navigation
# ----------------------------------------------------------------------
setopt auto_cd auto_pushd pushd_ignore_dups pushdminus

# Dir aliases worth keeping from omz lib/directories.zsh.
alias -g ...='../..'
alias -g ....='../../..'
alias -- -='cd -'
alias md='mkdir -p'

# ----------------------------------------------------------------------
# Misc shell behaviour
# ----------------------------------------------------------------------
setopt multios long_list_jobs interactivecomments

# ----------------------------------------------------------------------
# Completion (compinit)
# ----------------------------------------------------------------------
mkdir -p "$HOME/.cache/zsh"
autoload -Uz compinit
compinit -i -d "$HOME/.cache/zsh/zcompdump"

zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}'
zstyle ':completion:*' use-cache yes
zstyle ':completion:*' cache-path "$HOME/.cache/zsh"
zstyle ':completion:*:cd:*' tag-order local-directories directory-stack path-directories
zstyle ':completion:*' list-colors ''

# ----------------------------------------------------------------------
# Keybindings (emacs mode + history prefix search + edit-command-line)
# ----------------------------------------------------------------------
bindkey -e
bindkey '^E' end-of-line
bindkey '^[[C' autosuggest-accept

autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

# Type a prefix + ↑ to history-search by that prefix. The omz
# killer-feature most users notice when it's gone.
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search

# ----------------------------------------------------------------------
# Modular config files (~/.zsh/*.zsh)
#
# Loaded in lexical order. OS-specific files were already sourced above.
# ----------------------------------------------------------------------
for file in "$HOME"/.zsh/*.zsh; do
  case "${file:t}" in
    homebrew.zsh|os-darwin.zsh) continue ;;
  esac
  source "$file"
done


# ----------------------------------------------------------------------
# Third-party plugins (cloned by stow-all.py --apply at pinned refs to
# ~/.local/share/zsh-plugins/<name>/; see ZSH_PLUGINS_DIR above)
# ----------------------------------------------------------------------
[[ -f "$ZSH_PLUGINS_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh" ]] \
  && source "$ZSH_PLUGINS_DIR/zsh-autosuggestions/zsh-autosuggestions.zsh"

# fzf shell integration: Ctrl-T file picker, Ctrl-R history, Esc-c cd
# picker, ** completion trigger (kill **<TAB>, ssh **<TAB>, etc.).
command -v fzf >/dev/null && eval "$(fzf --zsh)"

# zoxide: z <pat> jumps to scored dir, zi opens an interactive picker.
command -v zoxide >/dev/null && eval "$(zoxide init zsh)"

# Syntax highlighting must be sourced LAST per upstream docs (it wraps zle widgets).
[[ -f "$ZSH_PLUGINS_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh" ]] \
  && source "$ZSH_PLUGINS_DIR/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
