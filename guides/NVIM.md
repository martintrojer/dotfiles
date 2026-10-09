# Neovim Learning Guide

Companion to [`nvim/README.md`](../nvim/README.md). 0.12 builtins +
fzf-lua + Oil + a small set of mini.nvim modules. The whole
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

Git work happens outside Neovim (jj, git, and `debrief` in the shell). Inside
nvim there are just fzf pickers for fast search.

- `<leader>gf`, `<leader>gc`, `<leader>gh`, `<leader>gb` — Git pickers (status, commits, buffer history, blame)

```quiz
[[questions]]
q = "Which mapping opens the fzf git blame picker?"
options = ["`<leader>gb`", "`<leader>gh`", "`<leader>gf`"]
answer = 0
why = "`<leader>gb` is blame; `gh` is buffer history and `gf` is status."
```

## fzf-lua pickers (`<leader>f`)

One picker UI handles files, buffers, LSP jumps, diagnostics, and recent
directories. The leader namespace is split on purpose: `<leader>f` is for
*finding things* (pickers), `<leader>s` is for *searching content* (grep).

- `<leader>ff` — files; `<leader>fF` — VCS files
- `<leader>fb` — buffers; `<leader>fo` — recent files; `<leader>fl` — buffer lines
- `<leader>fh` — help tags; `<leader>fk` — keymaps; `<leader>fc` — commands
- `<leader>fj` — jumps; `<leader>fC` — changes; `<leader>fm` — marks; `<leader>f,` — registers
- `<leader>fq` / `<leader>fQ` — quickfix / location list
- `<leader>f.` — resume last picker
- `<leader>fz` — zoxide directories → opens in Oil
- `<leader>fd` / `<leader>fD` — document / workspace diagnostics
- `<leader>fs` / `<leader>fS` — document / workspace LSP symbols
- LSP via picker: `gd` definitions, `gr` references, `gi` implementations,
  `gy` type definitions; `<leader>ci`, `<leader>co`, `<leader>cF`
- inside a picker: `Ctrl`+`g` toggles fuzzy / regex matching

```quiz
[[questions]]
q = "Which mapping resumes the last picker?"
options = ["`<leader>fo`", "`<leader>f.`", "`<leader>fr`"]
answer = 1
why = "`<leader>f.` resumes the last picker."

[[questions]]
q = "What is the `<leader>f` vs `<leader>s` split about?"
options = [
  "`f` is for files only, `s` is for symbols",
  "`f` is for fzf pickers (find things), `s` is for search/grep over content",
  "They are aliases for the same commands",
]
answer = 1
why = "Pickers live under `<leader>f`; grep / search live under `<leader>s`."

[[questions]]
q = "Inside a picker, what toggles fuzzy vs regex matching?"
options = ["`Tab`", "`Ctrl`+`r`", "`Ctrl`+`g`"]
answer = 2
why = "`Ctrl`+`g` switches fuzzy and regex modes inside a picker."
```

## Search & grep (`<leader>s`)

Grep lives under `<leader>s` so the picker namespace stays clean.

- `<leader>sg` — live grep (rg); `<leader>s/` — resume live grep
- `<leader>sG` — Git grep
- `<leader>sw` — grep word under cursor

```quiz
[[questions]]
q = "Which mapping greps the word under the cursor?"
options = ["`<leader>fw`", "`<leader>sw`", "`<leader>*`"]
answer = 1
why = "Grep / search live under `<leader>s`; pickers live under `<leader>f`."
```

## Oil and terminal

These replace a file tree and keep shell workflows inside Neovim.

- `-` — open Oil on the parent directory
- in Oil: edit filenames to rename, delete lines to delete files, yank/paste
  to copy or move, `:w` applies changes
- in Oil: `Enter` opens entry, `Alt`+`h` horizontal split, `g.` toggles hidden
- `Ctrl`+`/` — toggle terminal split
- `Esc` `Esc` — leave terminal insert mode
- `Ctrl`+`h`/`j`/`k`/`l` — move to tmux panes

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
```

## mini.nvim modules

Most editor niceties come from one plugin suite, which keeps the API and
behavior consistent.

**Surround (`mini.surround`)** — the "around" mnemonic: `s` + verb + target.

- `sa{motion}{char}` — *surround add*: wrap a text object with `char`.
  Examples: `saiw)` wraps the inner word in `()`; `saW"` wraps the WORD in
  `""`; `sa$\`` wraps to end-of-line in backticks; `sa2aw]` wraps two `aw`
  objects in `[]`. Visual mode: select then `sa)`.
- `sd{char}` — *surround delete*: strip the nearest `char` pair around the
  cursor. `sd)` removes parens.
- `sr{from}{to}` — *surround replace*: swap one pair for another. `sr({`
  turns `(x)` into `{x}`; `sr)>` turns `(x)` into `<x>`.
- `sf` / `sF` — find next / previous surrounding to the right / left.
- `sh` — highlight the surrounding pair.
- Special targets beyond literal chars: `f` = function call (`saiwf` then
  type `print` → `print(word)`), `t` = HTML/XML tag, `?` = prompt for
  arbitrary left/right strings.

**Navigation — `mini.bracketed`**

- `[b` / `]b` buffers, `[d` / `]d` diagnostics, `[q` / `]q` quickfix,
  `[l` / `]l` location list, `[x` / `]x` conflict markers
- the suite also covers `[c`/`]c` comments, `[f`/`]f` files in cwd,
  `[j`/`]j` jumplist, `[t`/`]t` treesitter nodes, `[w`/`]w` windows,
  `[y`/`]y` yanks — capital variants jump to first / last

**Display & feedback**

- `mini.hipatterns` highlights `TODO:`, `FIX:`, `FIXME:`, `HACK:`, `NOTE:`
  keywords and `#rrggbb` hex colors inline
- `mini.indentscope` draws a thin guide along the current indent block
- `mini.cursorword` underlays the word under the cursor
- `mini.trailspace` highlights trailing whitespace
- `mini.notify` backs `vim.notify()`; `:lua MiniNotify.show_history()` opens its history
- `mini.tabline` shows buffers as tabs; `mini.statusline` renders mode,
  diagnostics, LSP, file info, location
- `mini.icons` provides file / git / LSP icons used across pickers and UI
- `mini.starter` is the start screen on bare `nvim`
- `mini.clue` shows prefix-key hints after ~300 ms (the `<leader>f` /
  `<leader>s` / `<leader>g` group labels you see come from here)

```quiz
[[questions]]
q = "How do you wrap the inner WORD under the cursor in double quotes?"
options = ["`saW\"`", "`ysW\"`", "`s\"W`"]
answer = 0
why = "`mini.surround` uses `sa` + text object + char, so `sa` + `W` + `\"` wraps the inner WORD in `\"\"`."

[[questions]]
q = "How do you swap surrounding `(...)` for `{...}` on the current pair?"
options = ["`sd({`", "`sr({`", "`sa({`"]
answer = 1
why = "`sr` is *surround replace*: `sr` + from-char + to-char."

[[questions]]
q = "What does the `f` target do in mini.surround (e.g., `saiwf`)?"
options = [
  "Wraps the text object in a literal `f`/`f` pair",
  "Wraps the text object in a function call — you're prompted for the function name",
  "Folds the text object",
]
answer = 1
why = "`f` is the function-call surround: `saiwf` then `print` produces `print(word)`. `t` is the HTML/XML tag equivalent."

[[questions]]
q = "Which mini module highlights `TODO:` / `FIX:` / `NOTE:` keywords inline?"
options = ["`mini.hipatterns`", "`mini.cursorword`", "`mini.indentscope`"]
answer = 0
why = "`mini.hipatterns` matches the keyword patterns and also colorizes `#rrggbb` hex values."
```

## Markdown and notes

Notes are read, searched and browsed in `ramble`, not nvim. When you do hand
edit a markdown file here, the `mdroots` language server attaches, so the stock
LSP maps work on links: `gd` follows one, `gr` lists references, `K` previews
the target.

```quiz
[[questions]]
q = "Where do you read, search and follow links between notes?"
options = ["nvim", "`ramble` (`m`)", "`debrief` (`d`)"]
answer = 1
why = "ramble is the markdown reader and notes front end; debrief reviews diffs; nvim is kept for occasional hand edits."
```

## Workflow extras

The glue: undo history, per-project config, and plugin maintenance.

- `<leader>u` — toggle the bundled `nvim.undotree` panel; `q` to close
- buffers `:checktime` themselves on `FocusGained` / `BufEnter` (combined
  with `autoread`) so external edits show up without manual `:e`
- drop a `.nvim.lua` in the project root for trusted local overrides (`exrc`)
- `:PackUpdate` — update plugins; `:LspInfo` — show attached LSPs (alias for `:checkhealth vim.lsp`);
  `:TSUpdate` — update treesitter parsers; `:TSSync` — install any parser
  from the wired-up language list (`bash`, `go`, `haskell`, `lua`, `python`,
  `rust`, `tsx`, `markdown`, ...) that's missing from the local install
- `:messages` — review past messages

```quiz
[[questions]]
q = "Which file enables per-project config overrides?"
options = ["`.exrc.lua`", "`nvim.local.lua`", "`.nvim.lua`"]
answer = 2
why = "Per-project overrides go in a trusted `.nvim.lua` file at the project root."

[[questions]]
q = "What does `:TSSync` do that `:TSUpdate` doesn't?"
options = [
  "Nothing — they're aliases",
  "Installs any parser from the config's curated language list that's missing locally, then exits",
  "Syncs treesitter highlights with the current colorscheme",
]
answer = 1
why = "`:TSSync` is a custom command that diff's the wired-up `ts_parsers` list against installed parsers and installs the gap; `:TSUpdate` upgrades already-installed parsers."

[[questions]]
q = "Which mapping opens the undo-history tree?"
options = ["`<leader>uu`", "`<leader>u`", "`U`"]
answer = 1
why = "`<leader>u` runs `:Undotree` (the bundled `nvim.undotree`); `q` closes the panel."
```
