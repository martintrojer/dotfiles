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
diff/log helpers for drill-down. `redline.nvim` wires `mini.diff` and the
bundled `nvim.difftool` into the statusline / signs so all three Git surfaces
share the same colors and indicators.

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
  `gy` type definitions; `<leader>ca`, `<leader>ci`, `<leader>co`, `<leader>cF`
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
why = "Pickers live under `<leader>f`; grep / search / replace flows live under `<leader>s`."

[[questions]]
q = "Inside a picker, what toggles fuzzy vs regex matching?"
options = ["`Tab`", "`Ctrl`+`r`", "`Ctrl`+`g`"]
answer = 2
why = "`Ctrl`+`g` switches fuzzy and regex modes inside a picker."
```

## Search & grep (`<leader>s`)

Grep, TODO scans, semantic search, and grep-driven search/replace all live
under `<leader>s` so the picker namespace stays clean.

- `<leader>sg` — live grep (rg); `<leader>s/` — resume live grep
- `<leader>sG` — Git grep
- `<leader>sw` — grep word under cursor
- `<leader>st` / `<leader>sT` — TODO / FIX grep (buffer dir / repo root)
- `<leader>sr` — grep → select with `Tab` → `Enter` → `cfdo` substitution
- `<leader>sR` — replace across the current quickfix list
- `<leader>sv` (n+v) / `<leader>sV` — vecgrep semantic search / live mode
- `<leader>sX` — reindex vecgrep

```quiz
[[questions]]
q = "Which mapping greps the word under the cursor?"
options = ["`<leader>fw`", "`<leader>sw`", "`<leader>*`"]
answer = 1
why = "Grep / search live under `<leader>s`; pickers live under `<leader>f`."

[[questions]]
q = "Which mapping starts semantic (vecgrep) search?"
options = ["`<leader>fv`", "`<leader>sv`", "`<leader>sg`"]
answer = 1
why = "Semantic search is `<leader>sv` (and works in visual mode for the selection)."

[[questions]]
q = "What does `<leader>sr` do?"
options = [
  "Resume the last picker",
  "Live grep, then drive a `cfdo` substitution across the matched files",
  "Reindex vecgrep",
]
answer = 1
why = "`<leader>sr` is the grep → quickfix → `cfdo` search/replace flow; `<leader>sX` is the vecgrep reindex."
```

## Oil, terminal, jj-fugitive, and sl-fugitive

These replace a file tree, keep shell workflows inside Neovim, and cover
Jujutsu and Sapling/Mercurial through the same `fugitive`-style command
interface.

- `-` — open Oil on the parent directory
- in Oil: edit filenames to rename, delete lines to delete files, yank/paste
  to copy or move, `:w` applies changes
- in Oil: `Enter` opens entry, `Alt`+`h` horizontal split, `g.` toggles hidden
- `Ctrl`+`/` — toggle terminal split
- `Esc` `Esc` — leave terminal insert mode
- `Ctrl`+`h`/`j`/`k`/`l` — move to tmux panes
- `:J` — JJ log; `:J status`, `:J diff`, `:J describe`, `:J bookmark`,
  `:J annotate`
- `:S` — Sapling/hg log via `sl-fugitive`; `<leader>gs` runs it from the VCS
  root and includes Phabricator forge links for `fbsource` / `infer`
- both fugitives default to opening in a new tab and share the same key
  conventions; `q` closes any of their split buffers

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

[[questions]]
q = "Which command opens the Sapling/Mercurial log via sl-fugitive?"
options = ["`:Sap`", "`:S`", "`:Hg`"]
answer = 1
why = "`sl-fugitive` registers a single `:S` Ex command modelled on `:G` / `:J`; `<leader>gs` invokes it from the VCS root."
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

**Motion & editing**

- `gS` — split or join arguments (`mini.splitjoin`)
- `Alt`+`j` / `Alt`+`k` — move lines or visual selection (`mini.move`)
- `Alt`+`h` / `Alt`+`l` — indent left / right (`mini.move`)
- `mini.pairs` auto-closes brackets and quotes

**Navigation — `mini.bracketed`**

- `[b` / `]b` buffers, `[d` / `]d` diagnostics, `[q` / `]q` quickfix,
  `[l` / `]l` location list, `[x` / `]x` conflict markers
- the suite also covers `[c`/`]c` comments, `[f`/`]f` files in cwd,
  `[j`/`]j` jumplist, `[t`/`]t` treesitter nodes, `[w`/`]w` windows,
  `[y`/`]y` yanks — capital variants jump to first / last

**Display & feedback**

- `mini.diff` shows gutter signs; `<leader>go` toggles full inline overlay
- `mini.git` powers `<leader>gi` (git inspect at cursor) and the log / diff
  / show / blame splits; `q` closes any of them
- `mini.hipatterns` highlights `TODO:`, `FIX:`, `FIXME:`, `HACK:`, `NOTE:`
  keywords and `#rrggbb` hex colors inline
- `mini.indentscope` draws a thin guide along the current indent block
- `mini.cursorword` underlays the word under the cursor
- `mini.trailspace` highlights trailing whitespace (and `:w` trims it)
- `mini.notify` backs `vim.notify()`; `<leader>en` opens history
- `mini.tabline` shows buffers as tabs; `mini.statusline` renders mode, git,
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

## Notes & writing (`<leader>z`, `<leader>p`, `<leader>t`)

Notes go through `zk-nvim` against `~/notes` (`vim.g.notes_path`).
`render-markdown.nvim` is **off by default** — markdown buffers stay raw and
editable. `<leader>pr` toggles a glow-like read mode per window: it enables
`render-markdown`, hides line numbers / signs, raises `conceallevel`, and
makes the buffer read-only. The `<leader>p` and `<leader>t` namespaces are
markdown-buffer-only (registered via `after/ftplugin/markdown.lua` and a
buffer-local `mini.clue` group).

**zk-nvim — notes (`<leader>z`)**

- `<leader>zn` — new note in the `inbox` group (prompts for title)
- `<leader>zN` — new permanent note (prompts for title, no group)
- `<leader>zw` — weekly journal entry (`journal` group)
- `<leader>zf` — find notes, sorted by modified
- `<leader>zs` — search notes (prompts for query, full-text via zk)
- `<leader>zz` — browse by tag
- `<leader>zl` / `<leader>zb` — outgoing links / backlinks for the current note
- `[[` (insert mode, markdown only) — `:ZkInsertLink` to pick & link a note

**Markdown preview (`<leader>p`, markdown only)**

- `<leader>pp` — `nabla.nvim` popup that renders inline LaTeX / math as
  Unicode ASCII art, no external tooling required
- `<leader>pr` — toggle markdown read mode (window-scoped): turns on
  `render-markdown` for headings/lists/code fences/checkboxes, hides UI
  chrome, and locks the buffer read-only. Following a wikilink in the same
  window keeps read mode active and re-locks the new buffer.
- `render-markdown.nvim` defaults: `html` rendering disabled (raw `<tag>`s
  stay visible), link concealment disabled (works around a wrap+conceal
  layout bug), and `anti_conceal` disabled so the cursor line stays pretty

**Markdown tools (`<leader>t`, markdown only)**

- `<leader>tt` — toggle a `- [ ]` / `- [x]` checkbox at the cursor
- `<leader>td` — insert `## YYYY-MM-DD` at cursor
- `<leader>ti` — insert elapsed time + counter since the timestamp timer started
- `<leader>tr` — reset the timestamp timer and counter
- `<leader>tc` — disable the timestamp counter (keep elapsed time only)
- `<leader>tf` — insert a flash-card scaffold `PROMPT : RESPONSE 🧠 #tag`
- `<leader>tv` / `<leader>tV` — edit repo-local Vale accept / reject vocab
  (auto-creates `.vale.ini` and `Local` vocabulary files at the VCS root)

```quiz
[[questions]]
q = "Where do new notes from `<leader>zn` get created?"
options = [
  "In the current buffer's directory",
  "Under `~/notes` (the `vim.g.notes_path`) in the `inbox` group",
  "In a temporary scratch buffer that you save manually",
]
answer = 1
why = "`<leader>zn` calls `ZkNew` against `vim.g.notes_path` with `group = 'inbox'`; `<leader>zN` is the same without a group."

[[questions]]
q = "In a markdown buffer, what does `[[` in insert mode do?"
options = [
  "Inserts two literal brackets",
  "Triggers `:ZkInsertLink` to pick another note and insert a wiki link",
  "Opens the LaTeX preview",
]
answer = 1
why = "`after/ftplugin/markdown.lua` maps `[[` to `:ZkInsertLink` so wiki-link insertion stays one keystroke."

[[questions]]
q = "What does `<leader>pp` do in a markdown buffer?"
options = [
  "Prints the buffer",
  "Renders the LaTeX / math expression at the cursor as a Unicode popup via `nabla.nvim`",
  "Toggles `render-markdown.nvim`",
]
answer = 1
why = "`nabla.nvim` renders math as ASCII art in a floating popup; `<leader>pr` is the toggle for `render-markdown` read mode."
```

## Workflow extras

The glue: quickfix-based replace, format-on-save, undo history, repo-local
Vale vocabulary, per-project config, and plugin maintenance.

- `<leader>sr` — find matches; `Tab` multi-select; `Enter` send to quickfix
- `<leader>sR` — `:cfdo %s/old/new/gc` across quickfix files
- `<leader>u` — toggle the bundled `nvim.undotree` panel; `q` to close
- `<leader>em` / `<leader>en` — messages history / `mini.notify` history; `q`
  in either buffer returns you to the previous one
- format-on-save runs on every `:w` via `lua/format_on_save.lua`: LSP
  formatter if any attached client supports it, else a CLI fallback
  selected by filetype (`prettier` for markdown / json / jsonc / json5 /
  yaml / html / css / scss / less / graphql / vue, `stylua` for lua,
  `shfmt` for sh / bash — zsh deliberately not formatted, shfmt would
  mangle it), then `MiniTrailspace` trims trailing whitespace and blank
  lines. Missing CLI binaries are silent no-ops, so the same config
  works across machines that don't have every formatter installed. No
  `conform.nvim` / `none-ls` — ~30 lines of glue, no plugin
- manual format on `<leader>cf`
- inlay hints auto-enable on `LspAttach` for any server that supports them;
  `<leader>ch` toggles them per-buffer
- buffers `:checktime` themselves on `FocusGained` / `BufEnter` (combined
  with `autoread`) so external edits show up without manual `:e`
- drop a `.nvim.lua` in the project root for trusted local overrides (`exrc`)
- `:PackUpdate` — update plugins; `:LspInfo` — show attached LSPs;
  `:TSUpdate` — update treesitter parsers; `:TSSync` — install any parser
  from the wired-up language list (`bash`, `go`, `haskell`, `lua`, `python`,
  `rust`, `tsx`, `markdown`, ...) that's missing from the local install
- `:messages` — review past notifications (or `<leader>em` for the same in a
  scratch buffer with `q` to close)

```quiz
[[questions]]
q = "Which mapping opens repo-local Vale accepted words?"
options = ["`<leader>tf`", "`<leader>tv`", "`<leader>tr`"]
answer = 1
why = "Accepted-word editing for the local Vale vocabulary is on `<leader>tv` (markdown buffers only)."

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
