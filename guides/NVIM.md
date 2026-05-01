# Neovim Learning Guide

Companion to [`nvim/README.md`](../nvim/README.md). 0.12 builtins +
fzf-lua + Oil + jj-fugitive + a small set of mini.nvim modules. The whole
config is intentionally close to stock Neovim with deliberate plugin choices.

## 0.12 builtins

Comments, completion, snippets, URL open, LSP defaults, and the changelist all
come from Neovim itself.

- `gcc` — toggle current-line comment
- `gc` (visual mode) — comment the selection
- `gcap` — comment a paragraph
- `Ctrl`+`n` / `Ctrl`+`p` — move completion selection
- `Ctrl`+`y` accept; `Ctrl`+`e` dismiss
- `Ctrl`+`x` `Ctrl`+`o` omni; `Ctrl`+`x` `Ctrl`+`f` file paths
- `van` — expand to parent treesitter node
- `vin` — shrink inward
- `gx` — open URL under cursor
- `g;` — older change; `g,` — newer change
- `<leader>fC` — change-list picker
- LSP defaults: `grn` rename, `gra` code action, `grr` references, `grt` type
  definition, `grx` run code lens, `gO` document symbols
- diagnostics: `[d` / `]d` navigate, `<leader>ee` float, `<leader>el` location
  list
- quickfix / loclist: `:copen`, `:lopen`, `:cnext`, `:lnext`, `[q` / `]q`,
  `[l` / `]l` (bracket mappings come from `mini.bracketed`)
- handy builtins: `:wall ++p` (save all, create parent dirs), `:help!`,
  `:iput`, `:uniq`, `:restart`, `:messages`

```quiz
[[questions]]
q = "Which mapping toggles a comment on the current line?"
options = ["`gc`", "`gcc`", "`cc`"]
answer = 1
why = "Current-line commenting is builtin, and the mapping is `gcc`."

[[questions]]
q = "Which mapping opens the URL under the cursor?"
options = ["`gr`", "`go`", "`gx`"]
answer = 2
why = "`gx` opens the URL under the cursor."

[[questions]]
q = "Which command saves all buffers and auto-creates parent directories?"
options = ["`:wall ++p`", "`:wa!`", "`:restart`"]
answer = 0
why = "`:wall ++p` writes all modified buffers and creates missing parent directories."
```

## Source control

Three layers: `lazygit` for big operations, fzf pickers for fast search, and
diff/log helpers for drill-down.

- `<leader>gg` — lazygit
- `<leader>gf`, `<leader>gc`, `<leader>gh`, `<leader>gb`, `<leader>gB`,
  `<leader>gS`, `<leader>gT` — Git pickers (files, commits, hunks, branches,
  blame, stash, tags)
- `<leader>gl` — repo log with stats
- `<leader>gi` — on a commit opens the full diff; on a hunk jumps to the
  source line
- `<leader>gd` — repo diff in a `mini.git` split
- `<leader>gU` — unified current-file diff
- `<leader>gD` — side-by-side current-file diff
- `<leader>go` — toggle `mini.diff` overlay

```quiz
[[questions]]
q = "Which mapping opens lazygit?"
options = ["`<leader>gg`", "`<leader>gf`", "`<leader>gj`"]
answer = 0
why = "lazygit lives on `<leader>gg`."

[[questions]]
q = "Which mapping shows the current file in a side-by-side diff view?"
options = ["`<leader>gd`", "`<leader>gU`", "`<leader>gD`"]
answer = 2
why = "The side-by-side current-file diff is `<leader>gD`."

[[questions]]
q = "Which mapping toggles the inline diff overlay?"
options = ["`<leader>gi`", "`<leader>go`", "`<leader>gl`"]
answer = 1
why = "`mini.diff`'s overlay toggle is `<leader>go`."
```

## fzf-lua

One picker UI handles files, grep, LSP jumps, diagnostics, buffers, and
recent directories.

- `<leader>ff` — files; `<leader>fg` — live grep; `<leader>fG` — Git grep
- `<leader>fw` — grep word under cursor
- `<leader>ft` / `<leader>fT` — TODO / FIX grep
- `<leader>fv` / `<leader>fV` — semantic search
- `<leader>fb` — buffers; `<leader>fo` — recent files; `<leader>fl` — buffer lines
- `<leader>fh` — help tags
- `<leader>f.` — resume last picker
- `<leader>fz` — zoxide directories → opens in Oil
- LSP via picker: `gd` definitions, `gr` references, `gi` implementations,
  `gy` type definitions; `<leader>ca`, `<leader>ci`, `<leader>co`, `<leader>cF`
- `<leader>fd`, `<leader>fs` — diagnostics, document symbols
- inside a picker: `Ctrl`+`g` toggles fuzzy / regex matching

```quiz
[[questions]]
q = "Which mapping resumes the last picker?"
options = ["`<leader>fo`", "`<leader>f.`", "`<leader>fr`"]
answer = 1
why = "`<leader>f.` resumes the last picker."

[[questions]]
q = "Which mapping starts semantic search?"
options = ["`<leader>fv`", "`<leader>fG`", "`<leader>fs`"]
answer = 0
why = "Semantic search uses vecgrep on `<leader>fv`."

[[questions]]
q = "Inside a picker, what toggles fuzzy vs regex matching?"
options = ["`Tab`", "`Ctrl`+`r`", "`Ctrl`+`g`"]
answer = 2
why = "`Ctrl`+`g` switches fuzzy and regex modes inside a picker."
```

## Oil, terminal, and jj-fugitive

These replace a file tree, keep shell workflows inside Neovim, and cover the
main Jujutsu commands.

- `-` — open Oil on the parent directory
- in Oil: edit filenames to rename, delete lines to delete files, yank/paste
  to copy or move, `:w` applies changes
- in Oil: `Enter` opens entry, `Alt`+`h` horizontal split, `g.` toggles hidden
- `Ctrl`+`/` — toggle terminal split
- `Esc` `Esc` — leave terminal insert mode
- `Ctrl`+`h`/`j`/`k`/`l` — move to tmux panes
- `:J` — JJ log
- `:J status`, `:J diff`, `:J describe`, `:J bookmark`, `:J annotate`

```quiz
[[questions]]
q = "Which mapping opens Oil?"
options = ["`_`", "`-`", "`go`"]
answer = 1
why = "A single dash opens Oil on the parent directory."

[[questions]]
q = "After editing filenames in Oil, how do you apply the changes?"
options = ["`:OilSave`", "`Enter`", "`:w`"]
answer = 2
why = "Oil applies filesystem edits when you write the buffer."

[[questions]]
q = "What does plain `:J` open?"
options = ["JJ log", "Git status", "Bookmarks"]
answer = 0
why = "Plain `:J` opens the JJ log by default."
```

## mini.nvim modules

Most editor niceties come from one plugin suite, which keeps the API and
behavior consistent.

- `sa` / `sd` / `sr` — add / delete / replace surroundings
- `gS` — split or join arguments
- `Alt`+`j` / `Alt`+`k` — move lines
- `Alt`+`h` / `Alt`+`l` — indent left / right
- `mini.pairs` auto-closes brackets and quotes
- `[b` / `]b` buffers, `[d` / `]d` diagnostics, `[q` / `]q` quickfix,
  `[l` / `]l` location list, `[x` / `]x` conflict markers
- `mini.diff` shows gutter signs; `<leader>go` toggles full overlay
- `mini.notify` backs `vim.notify()`; `<leader>en` opens history
- `mini.tabline` shows buffers as tabs
- `mini.statusline` renders mode, git, diagnostics, LSP, file info, location
- `mini.cursorword` highlights the word under the cursor
- `mini.trailspace` highlights and trims trailing whitespace
- `mini.starter` is the start screen
- `mini.clue` shows prefix-key hints after a short delay

```quiz
[[questions]]
q = "Which mapping replaces an existing surrounding?"
options = ["`sa`", "`sr`", "`sd`"]
answer = 1
why = "`sr` replaces one surrounding with another."

[[questions]]
q = "Which mapping toggles split/join formatting?"
options = ["`gS`", "`gs`", "`sj`"]
answer = 0
why = "`gS` toggles split/join formatting for argument lists and similar structures."

[[questions]]
q = "Which mapping opens notification history?"
options = ["`<leader>ee`", "`<leader>em`", "`<leader>en`"]
answer = 2
why = "Notification history is exposed on `<leader>en`."
```

## Workflow extras

The glue: quickfix-based replace, format-on-save, repo-local Vale vocabulary,
per-project config, and plugin maintenance.

- `<leader>fr` — find matches; `Tab` multi-select; `Enter` send to quickfix
- `<leader>fR` — `:cfdo %s/old/new/gc` across quickfix files
- format-on-save runs on every `:w`: LSP format if available, trim trailing
  whitespace, trim trailing blank lines
- manual format on `<leader>cf`
- `<leader>tv` / `<leader>tV` — repo-local Vale accepted / rejected words.
  Finds the nearest `.git`, `.jj`, or `.hg` root and creates `.vale.ini` and
  local vocabulary files if needed.
- drop a `.nvim.lua` in the project root for trusted local overrides
- `:PackUpdate` — update plugins
- `:LspInfo` — show attached LSPs
- `:TSUpdate` — update treesitter parsers
- `:messages` — review past notifications

```quiz
[[questions]]
q = "Which mapping opens repo-local Vale accepted words?"
options = ["`<leader>tf`", "`<leader>tv`", "`<leader>tr`"]
answer = 1
why = "Accepted-word editing for the local Vale vocabulary is on `<leader>tv`."

[[questions]]
q = "Which file enables per-project config overrides?"
options = ["`.exrc.lua`", "`nvim.local.lua`", "`.nvim.lua`"]
answer = 2
why = "Per-project overrides go in a trusted `.nvim.lua` file at the project root."

[[questions]]
q = "Which command updates all plugins?"
options = ["`:PackUpdate`", "`:TSUpdate`", "`:LspInfo`"]
answer = 0
why = "`:PackUpdate` updates plugins managed by `vim.pack`."
```
