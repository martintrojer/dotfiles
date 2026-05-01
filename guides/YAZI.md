# Yazi Learning Guide

Companion to [`yazi/README.md`](../yazi/README.md). Vim-like keys, visual mode,
tabs and tasks, fd / ripgrep / fzf / zoxide, bulk ops, and the repo's custom
goto keys.

## Basic movement and help

Yazi's defaults are intentionally Vim-like. This repo keeps that model and
adds only a handful of small custom keymaps on top.

- `j` / `k` — move down and up
- `h` — leave to the parent directory
- `l` — enter the hovered directory
- `gg` / `G` — jump to top / bottom
- `H` / `L` — back / forward in directory history
- `F1` or `~` — open help
- `q` — quit
- `Ctrl`+`c` — close the current tab (or quit if it is the last tab)
- `Esc` — cancel visual mode, selection, or search state
- `.` — toggle hidden files (this repo shows them by default)

The repo-local layout ratio is `[2, 4, 3]`, hidden files are shown by default,
and the Catppuccin Mocha flavor is enabled.

```quiz
[[questions]]
q = "Which key goes to the parent directory?"
options = ["`h`", "`l`", "`H`"]
answer = 0
why = "Yazi uses Vim-like h/l for leave/enter."

[[questions]]
q = "Which keys open help?"
options = ["`?` only", "`F1` or `~`", "`Ctrl-h`"]
answer = 1
why = "Both keys are bound to help in the default keymap."

[[questions]]
q = "What does `.` do in this setup?"
options = ["Create a file", "Toggle hidden files", "Open the task manager"]
answer = 1
why = "show_hidden defaults to true here, and `.` flips that visibility."
```

## Selection, visual mode, and bulk operations

The power move in Yazi is to think in terms of *selected files*, then apply
copy / cut / paste / delete / rename to the whole set.

- `Space` — toggle the current file and move down one row
- `Ctrl`+`a` — select all files
- `Ctrl`+`r` — invert selection
- `v` — visual mode (selection); `V` — visual unset mode
- `y` — yank (copy); `x` — yank as cut
- `p` — paste; `P` — force-overwrite paste
- `d` — trash selected files; `D` — permanently delete
- `r` — rename selected file(s); doubles as the bulk-rename entrypoint
- `-` — symlink yanked files (absolute); `_` — relative symlinks; `Ctrl`+`-` — hardlinks
- `Y` or `X` — clear yank state

```quiz
[[questions]]
q = "Which key enters visual selection mode?"
options = ["`v`", "`m`", "`Tab`"]
answer = 0
why = "`v` enters visual mode for file selection."

[[questions]]
q = "Which key starts a normal paste of yanked files?"
options = ["`y`", "`p`", "`P`"]
answer = 1
why = "`p` pastes the yanked set; uppercase `P` is the force-overwrite variant."

[[questions]]
q = "How do you select all files in the current view?"
options = ["`Ctrl`+`a`", "`A`", "`gg`"]
answer = 0
why = "`toggle_all --state=on` is bound to `Ctrl`+`a`."
```

## Search, fd, ripgrep, fzf, and zoxide

Yazi leans on external search tools rather than reinventing them.

- `s` — `search --via=fd` (filename search)
- `S` — `search --via=rg` (content search via ripgrep)
- `Ctrl`+`s` — cancel an ongoing search
- `/` and `?` — in-view find next / previous
- `z` — zoxide jump (built-in plugin)
- `Z` — fzf jump (built-in plugin)
- `g` then `Space` — interactive cd
- `g` `s` — go to the configured SFTP service `bubba`
- `g` `t` — jump to Trash via `trashd`
- `g` `l` — preview the current file via the shared pager-backed `m` command

```quiz
[[questions]]
q = "Which key searches file names via `fd`?"
options = ["`s`", "`S`", "`z`"]
answer = 0
why = "Lowercase `s` uses fd; uppercase `S` switches to ripgrep for content search."

[[questions]]
q = "Which key launches the built-in zoxide jump?"
options = ["`z`", "`Z`", "`g` `Space`"]
answer = 0
why = "Zoxide stays on lowercase `z` to match the shell binding; uppercase `Z` is fzf."

[[questions]]
q = "What does `g` then `t` do in this repo?"
options = ["Open a new tab", "Go to Trash", "Toggle the task manager"]
answer = 1
why = "The custom keymap uses the helper script to jump to the platform trash directory."
```

## Tabs and task manager

Two often-missed Yazi features: tabs and asynchronous task management. They
matter because copy / search / preview work continues in the background.

- `t` — create a new tab at the current working directory
- `1` … `9` — switch to a specific tab
- `[` / `]` — previous / next tab
- `{` / `}` — swap the current tab with its neighbors
- `w` — open the task manager
- inside tasks: `j` / `k` move; `Enter` inspects; `x` cancels; `Esc`, `w`, or `Ctrl`+`c` closes

Yazi is built around async I/O and background workers — file copies, previews,
searches, and uploads/downloads all keep progressing while you keep navigating.

```quiz
[[questions]]
q = "Which key opens the task manager?"
options = ["`w`", "`t`", "`~`"]
answer = 0
why = "`w` is the default `tasks:show` binding."

[[questions]]
q = "Which key creates a new tab at the current working directory?"
options = ["`T`", "`t`", "`Tab`"]
answer = 1
why = "Lowercase `t` creates a new tab with the current cwd."

[[questions]]
q = "Inside the task manager, what does `x` do?"
options = ["Exit Yazi", "Cancel the selected task", "Cut selected files"]
answer = 1
why = "The tasks view binds `x` to cancellation."
```

## Openers, previews, and custom repo keys

The repo keeps Yazi mostly stock and adds a few targeted helpers for clipboard
copying, remote navigation, markdown preview, and Trash access.

- `Enter` or `o` — open the selected file
- `O` or `Shift`+`Enter` — open interactively
- `Tab` — spot the hovered file for preview-related actions
- `c` `i` — copy file contents to the system clipboard via `clipf`
- `g` `s` — open the SFTP service named `bubba`
- `g` `l` — preview the current file via the shared pager-backed `m` command
- `g` `t` — go to Trash via `trashd`

`clipf` preserves MIME type. On macOS it uses `pbcopy` for text and special
handling for images / PDFs; on Wayland it uses `wl-copy`.

```quiz
[[questions]]
q = "Which custom key copies file contents to the clipboard?"
options = ["`c` `c`", "`c` `i`", "`y`"]
answer = 1
why = "The repo prepends `c` `i` to call `clipf` on the current file."

[[questions]]
q = "Which key opens selected files interactively?"
options = ["`O`", "`p`", "`l`"]
answer = 0
why = "Uppercase `O` is `open --interactive`."

[[questions]]
q = "What does `g` then `l` do here?"
options = ["Jump left", "Preview in pager", "Link the file"]
answer = 1
why = "The custom binding invokes the shared `m` command as a pager-backed preview action."
```

## Why this setup earns its place

Yazi is worth a guide here because it is both shortcut-heavy and
system-integrated: async workers, multi-tab state, a virtual filesystem
(including SFTP), and shell / tool integrations all reinforce each other.

What this repo customizes is small: Catppuccin Mocha flavor, hidden files
shown by default, a few custom keymaps rather than a totally rewritten
experience, and named VFS services for `bubba` and `pizero2`.

Mental model: Yazi is a fast async file-control plane. Navigate, select a set,
run an operation, let it continue in the background, and keep moving.

```quiz
[[questions]]
q = "Which statement best matches Yazi's value in this setup?"
options = [
  "A static file tree with almost no integrations",
  "An async file-control plane with tabs, selection, and tool integrations",
  "Just a color theme demo",
]
answer = 1
why = "The real value is the combination of fast navigation, bulk ops, and background work."

[[questions]]
q = "How heavily does this repo customize Yazi's defaults?"
options = [
  "It rewrites nearly every key",
  "It keeps Yazi mostly stock and adds a few targeted mappings",
  "It disables all default search features",
]
answer = 1
why = "The repo adds a handful of custom mappings rather than replacing the whole experience."

[[questions]]
q = "Why does Yazi's async task model matter for bulk operations?"
options = [
  "Because it lets heavy work continue while you keep navigating",
  "Because it disables previews",
  "Because it removes tabs",
]
answer = 0
why = "That is exactly why the task manager and background worker model are worth learning."
```
