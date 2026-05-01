# Zsh Learning Guide

Companion to [`zsh/README.md`](../zsh/README.md). Native compinit, native
keybindings, lexical file loading, and exactly two sourced third-party zsh
plugins — that's the whole shape of this shell.

## Startup model and layout

This config avoids framework layers. It uses native `compinit`, native
keybindings, lexical file loading, and exactly two sourced third-party plugins.

- `./dotfiles-sync --apply` stows `zsh/.zshrc` and `zsh/.zsh/` into `~/`
- it also clones `zsh-autosuggestions` and `zsh-syntax-highlighting` into
  `~/.local/share/zsh-plugins/` at pinned refs
- `~/.zsh/exports.zsh` — PATH, TERM, editor, pager, history env
- `~/.zsh/tools.zsh` — `mise` activation + custom prompt
- `~/.zsh/aliases.zsh` — eza aliases, global pipe aliases, wrappers
- `~/.zsh/git-aliases.zsh`, `jj-aliases.zsh`, `functions.zsh`
- plugin completion dirs are added to `fpath` *before* `compinit`
- Darwin-specific files are sourced first so PATH is correct before completion
  starts; then `~/.zsh/*.zsh` loads in lexical order

```quiz
[[questions]]
q = "How many third-party zsh plugins are sourced directly by this setup?"
options = ["2", "5", "15"]
answer = 0
why = "Only `zsh-autosuggestions` and `zsh-syntax-highlighting` are sourced directly."

[[questions]]
q = "Where are pinned zsh plugins cloned?"
options = [
  "`~/.oh-my-zsh/custom/plugins`",
  "`~/.local/share/zsh-plugins/`",
  "`~/.config/zsh/plugins/`",
]
answer = 1
why = "`dotfiles-sync` clones the plugins under `~/.local/share/zsh-plugins`."

[[questions]]
q = "What initializes completion in this config?"
options = ["antigen", "zplug", "native `compinit`"]
answer = 2
why = "The setup explicitly uses autoloaded `compinit` instead of a framework plugin manager."
```

## Aliases and navigation

The everyday shell layer is mostly a small set of aliases that actually earned
their keep in history: eza-backed listing, global pipeline shorthands,
directory helpers, and fd-based recursive search.

- `ls`, `ll`, `lla`, `la`, `lt`, `llt`, `lt5`, `llt5`, `l` — all go through
  `eza`. `lla` is the long listing including dotfiles.
- `F` — pipe to `fzf`
- `H` / `T` — pipe to `head` / `tail`
- `G` / `L` — pipe to `grep` / `less`
- `LL`, `NE`, `NUL` — redirection shortcuts
- `...`, `....`, `-`, `md` — survivors from the old OMZ directory helpers
- `ff` — recursive `fd` search for hidden files (excludes `.git` and `.jj`)
- `ffd` — directory-only recursive variant

```quiz
[[questions]]
q = "What does the global alias `F` expand to?"
options = ["`| fzf`", "`| fd`", "`| fmt`"]
answer = 0
why = "`F` is the pipe-to-fzf shorthand."

[[questions]]
q = "Which alias shows a long listing including dotfiles?"
options = ["`ll`", "`lla`", "`lt`"]
answer = 1
why = "The extra `a` is the hidden-file variant of the long listing."

[[questions]]
q = "What does `ff` search for?"
options = ["Directories only", "Git branches", "Files recursively"]
answer = 2
why = "`ff` wraps `fd` for hidden recursive file search while excluding `.git` and `.jj`."
```

## Git, JJ, and diff wrappers

The VCS aliases were trimmed with actual shell history instead of nostalgia.
Only the commands used often survived.

- Git: `g`, `ga`, `gb`, `gc`, `gco`, `gd`, `gf`, `gl`, `glg`, `gp`, `grb`, `gst`
  (and `gc!` for amend)
- JJ: `jja`, `jjb`, `jjd`, `jjdmsg`, `jje`, `jjgp`, `jjl`, `jjla`, `jjn`,
  `jjrb`, `jjsq`, `jjst` — `jjla` is the all-revisions log
- `gvd` → `git difftool --dir-diff --no-prompt --extcmd=nvdiff`
- `jvd` → `jj --no-pager diff --tool nvdiff`
- `nvdiff` opens Neovim's `:DiffTool` through the repo script

```quiz
[[questions]]
q = "What does `gvd` launch?"
options = [
  "git blame in vim",
  "git difftool with `nvdiff`",
  "git verbose diffstat only",
]
answer = 1
why = "`gvd` is the dir-diff wrapper that routes review through `nvdiff`."

[[questions]]
q = "Which alias shows the JJ log over `all()`?"
options = ["`jjla`", "`jjst`", "`jjd`"]
answer = 0
why = "`jjla` is the all-revisions log alias."

[[questions]]
q = "What does `gst` expand to?"
options = ["`git stash`", "`git show --stat`", "`git status`"]
answer = 2
why = "`gst` is the familiar `git status` alias that survived the audit."
```

## Interactive shell features and keybindings

Most interactivity comes from built-in zle widgets plus `fzf`, `zoxide`,
autosuggestions, and syntax highlighting layered on top.

- `↑` / `↓` — prefix-sensitive history search
- `Ctrl`+`E` — jump to end-of-line
- `Ctrl`+`X` `Ctrl`+`E` — open the current command line in `$EDITOR`
- `zsh-autosuggestions` shows ghost-text suggestions from history
- `Right Arrow` accepts the suggestion via the normal `forward-char` widget
- `zsh-syntax-highlighting` is sourced *last* (upstream requires it)
- `fzf --zsh` gives `Ctrl`+`T` file picker, `Ctrl`+`R` history search,
  `Esc` then `c` directory picker, and `**` completion triggers
- `zoxide init zsh` gives `z <pat>` and `zi`

```quiz
[[questions]]
q = "Which keybinding opens the current command line in your editor?"
options = ["`Ctrl`+`E`", "`Ctrl`+`X` `Ctrl`+`E`", "`Ctrl`+`R`"]
answer = 1
why = "`edit-command-line` is bound to `Ctrl`+`X` `Ctrl`+`E`."

[[questions]]
q = "How do you accept an autosuggestion?"
options = ["Press `Right Arrow`", "Press `Tab` twice", "Press `Ctrl`+`Space`"]
answer = 0
why = "Autosuggestions wrap zsh's `forward-char` widget, so `Right Arrow` accepts when a suggestion is present."

[[questions]]
q = "What does `Esc` then `c` come from?"
options = ["zoxide", "fzf shell integration", "syntax-highlighting"]
answer = 1
why = "fzf provides the cd picker on `Esc`-`c`."
```

## Functions and workflow helpers

The function layer stays small and practical: notes, a shared Neovim socket,
symlink-preserving moves, Yazi cwd handoff, history cleanup, and tmux
attach/create.

- `zknew` — prompt for a title and create a note in `$HOME/notes` via `zk`
- `nv` — open a file in the shared Neovim server at `/tmp/nvim.pipe` if it
  exists, otherwise start a listening instance
- `mvln` — move a file, then leave a symlink at the old path pointing to the
  new location
- `y` — launch Yazi, then `cd` to the selected directory on exit using a temp
  cwd file
- `rmhist <pattern>` — remove matching lines from `$HISTFILE` and reload it
- `tm` — runs `$HOME/.config/tmux/scripts/tms pick-and-connect`

```quiz
[[questions]]
q = "What does `tm` run?"
options = [
  "`tmux attach -t main`",
  "`$HOME/.config/tmux/scripts/tms pick-and-connect`",
  "`tmux new -s scratch`",
]
answer = 1
why = "`tm` is a thin wrapper around the repo's `tms` picker and attach flow."

[[questions]]
q = "What is the purpose of `nv`?"
options = [
  "Always spawn a brand new Neovim process",
  "Edit shell history",
  "Reuse a shared Neovim server if one is listening",
]
answer = 2
why = "`nv` connects to `/tmp/nvim.pipe` when present, otherwise starts the server."

[[questions]]
q = "What does `y` do after Yazi exits?"
options = [
  "Nothing special",
  "Changes the shell cwd to the selected directory when different",
  "Deletes the working directory",
]
answer = 1
why = "`y` reads Yazi's cwd file and `cd`s there if needed."
```

## Prompt, environment, boundaries, and updates

The prompt is hand-rolled, the environment favors stable defaults, OS-specific
tweaks stay in their own files, and plugin updates happen by bumping pinned
refs rather than using a live plugin manager.

- left prompt — shortened cwd + a yellow chevron
- right prompt — red error glyph when the last command failed
- inside a toolbox the prompt adds the toolbox name; over SSH it adds the host
- if outside tmux and the host terminfo is missing, `TERM` falls back to
  `xterm-256color`
- toolbox containers also force `xterm-256color`
- `bat` / `batcat` becomes `PAGER` / `MANPAGER` when available
- history sizes are set to `1,048,576` lines
- Darwin-specific behavior stays in `homebrew.zsh` and `os-darwin.zsh`
- to update the pinned plugins, bump `ZSH_PLUGINS` in `_dotfiles_sync/pins.py`
  and run `./dotfiles-sync --apply`
- `./dotfiles-sync --check` reports missing, unknown-ref, or drift issues

```quiz
[[questions]]
q = "What appears in the right prompt when the last command fails?"
options = ["Current git branch", "A red error glyph", "The full PATH"]
answer = 1
why = "`RPROMPT` shows a failure marker instead of hiding the last command state."

[[questions]]
q = "Which `TERM` value is forced inside toolbox containers?"
options = ["`screen-256color`", "`ghostty`", "`xterm-256color`"]
answer = 2
why = "Toolbox lacks host terminfo entries often enough that the config forces `xterm-256color` there."

[[questions]]
q = "How do you update the pinned zsh plugins?"
options = [
  "Run an interactive zsh plugin manager",
  "Edit `_dotfiles_sync/pins.py` and re-run `./dotfiles-sync --apply`",
  "Delete `~/.zshrc` and reboot",
]
answer = 1
why = "The repo uses pin updates, not a live zsh plugin manager."
```
