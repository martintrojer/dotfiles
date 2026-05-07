# eza-backed replacements for the standard ls family.
alias ls='eza --icons=auto'
alias ll='eza -l --icons=auto --group-directories-first'
alias lla='eza -la --icons=auto --group-directories-first'
alias la='eza -a --icons=auto --group-directories-first'
# Noise dirs to hide in `lt*` tree views.
# VCS:    .git .hg .jj .svn
# OS:     .DS_Store
# Python: .venv venv __pycache__ *.egg-info .eggs .ruff_cache .mypy_cache .pytest_cache .tox htmlcov
# Node:   node_modules .next .nuxt .svelte-kit .turbo .parcel-cache coverage
# Rust:   target
# Misc:   dist build .cache .direnv .idea
_EZA_TREE_IGNORE='.git|.hg|.jj|.svn|.DS_Store|node_modules|__pycache__|.venv|venv|*.egg-info|.eggs|.ruff_cache|.mypy_cache|.pytest_cache|.tox|htmlcov|coverage|target|dist|build|.cache|.next|.nuxt|.svelte-kit|.turbo|.parcel-cache|.direnv|.idea'
alias lt='eza -a --icons=auto --tree -L 3 -I $_EZA_TREE_IGNORE'
alias llt='eza -la --icons=auto --tree -L 3 -I $_EZA_TREE_IGNORE'
# Tree-list depth presets.
alias lt5='eza -a --icons=auto --tree -L 5 -I $_EZA_TREE_IGNORE'
alias llt5='eza -la --icons=auto --tree -L 5 -I $_EZA_TREE_IGNORE'
alias l='eza -lahG --icons=auto --no-permissions --no-user'

# General aliases
alias -g F='| fzf'
alias -g H='| head'
alias -g T='| tail'
alias -g G='| grep'
alias -g L='| less'
alias -g LL='2>&1 | less'
alias -g NE='2> /dev/null'
alias -g NUL='> /dev/null 2>&1'
alias serve='python3 -m http.server 8081'
alias td='cd "$(trashd)"'
alias gvd='git difftool --dir-diff --no-prompt --extcmd=nvdiff'
alias jvd='jj --no-pager diff --tool nvdiff'

# fd-backed recursive find helpers from the old OMZ setup.
if command -v fd >/dev/null 2>&1; then
  alias ff='fd --hidden --exclude .git --exclude .jj --type f'
  alias ffd='fd --hidden --exclude .git --exclude .jj --type d'
fi

# Common `less` misspelling (the only one with history hits).
alias elss='less'

# `m` (local-bin/.local/bin/m) wraps glow with a prettier prose-reflow pass
# so hard-wrapped paragraphs reflow to the current width. `mr` ("m raw") is
# the bypass: same `m` pipeline (style, less paging, truecolor pty, TERM
# override) minus the prettier preprocessing, for when prettier mangles a
# file or you want to see the source layout untouched.
alias mr='m --raw'
