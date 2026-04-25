# Neovim Config

Minimal Neovim 0.12 config. No framework, no plugin manager plugin — just `vim.pack`, 0.12 builtins, and mini.nvim.

## Philosophy

- **0.12 builtins first** — LSP, completion, commenting, formatting, snippets, node selection, URL open all use native nvim features
- **fzf-lua as the centre piece** — all file finding, grepping, LSP actions (definitions, references, code actions), buffer switching, and `vim.ui.select` go through fzf-lua. One consistent fuzzy interface for everything
- **mini.nvim as the plugin suite** — one repo, 17 modules, consistent API, zero external deps
- **vim.pack for plugin management** — builtin, lockfile, no bootstrap
- **Every line is understood** — no framework magic, no hidden keymaps, no surprise plugins

## Structure

```
init.lua                        — options, colorscheme, diagnostics, commands, autocommands
lua/
  plugins.lua                   — vim.pack.add + build hooks
  mini_setup.lua                — 17 mini modules + clue + statusline + notify
  starter.lua                   — start screen + logo + greeting
  keymaps/                      — keymaps split by domain
    init.lua                    — dispatcher: loads each submodule with shared `map` helper
    core.lua                    — editor, terminal, tmux nav, buffer mgmt
    find.lua                    — fzf-lua pickers
    git.lua                     — source control (lazygit, mini.git, jj-fugitive)
    search.lua                  — grep, search & replace, vecgrep
    notes.lua                   — zk + markdown helpers
    lsp.lua                     — LSP actions (definitions, references, code actions)
  lsp.lua                       — LSP server configs + enable
  tabterm.lua                   — tab-based terminal helper (used by lazygit, tuicr)
  history.lua                   — message and notification history viewers
  todos.lua                     — TODO grep + markdown todo toggle
  timestamps.lua                — elapsed time utility (markdown notes)
  async_run.lua                 — `:Sh` async shell with streaming output split
  git_diff.lua                  — side-by-side git diff helpers (used by `<leader>gD`)
  util.lua                      — shared helpers (VCS root detection, cwd helpers, etc.)
  lua_globals.lua               — lua-language-server globals list
after/ftplugin/
  markdown.lua                  — markdown-specific keymaps (nabla, zk link, cards, todos)
```

## Plugins (13 vim.pack entries)

### mini.nvim (17 modules from one repo)

| Module | Purpose |
|--------|---------|
| mini.bracketed | `[`/`]` navigation (buffers, diagnostics, quickfix) |
| mini.clue | Key clue popup on prefix keys |
| mini.cursorword | Highlight word under cursor |
| mini.diff | Git hunk signs in gutter + diff overlay |
| mini.git | Lightweight Git inspection + `:Git` command |
| mini.hipatterns | Highlight TODO/FIX/HACK/NOTE and hex colors inline |
| mini.icons | File/filetype icons |
| mini.indentscope | Indent scope guide line |
| mini.move | Move lines/selections with Alt-h/j/k/l |
| mini.notify | Floating notifications |
| mini.pairs | Auto-close brackets/quotes |
| mini.splitjoin | Toggle single-line / multi-line args |
| mini.starter | Start screen with recent files and actions |
| mini.statusline | Statusline (mode, git, diagnostics, LSP, position) |
| mini.surround | Add/delete/change surroundings |
| mini.tabline | Buffer tab bar |
| mini.trailspace | Highlight trailing whitespace |

### Other plugins (each earns its place)

| Plugin | Purpose | Why not builtin? |
|--------|---------|-----------------|
| catppuccin | Catppuccin Mocha theme | Unified theme across terminal, eza, bat, tmux, waybar |
| fzf-lua | Fuzzy finder + LSP actions | No builtin picker. Uses fzf binary. Also handles `vim.ui.select` |
| oil.nvim | File explorer as editable buffer | Nothing like it builtin — rename/move/delete by editing text |
| vim-tmux-navigator | Tmux pane navigation | Requires matching tmux config. No builtin tmux awareness |
| nvim-treesitter | Parser management | 0.12 ships treesitter runtime but needs this for parser install/update |
| fugitive-core.nvim | Shared VCS fugitive core | Own plugin. Common functionality extracted from jj-fugitive |
| jj-fugitive | Jujutsu VCS power tool | Own plugin. Primary workflow here; git support stays intentionally lightweight |
| redline.nvim | Inline review comments | Own plugin. Integrates with mini.git, `:DiffTool`, and jj-fugitive reviews |
| render-markdown.nvim | In-buffer markdown rendering | Headings, code blocks, tables, checkboxes via treesitter. Heavy markdown user |
| nabla.nvim | LaTeX formula popup preview (Unicode, no deps) |
| zk-nvim | Zettelkasten notes | Creates, finds, links, navigates notes via zk CLI. Core daily workflow |
| vecgrep.nvim | Semantic search | Own plugin. Local embeddings for meaning-based search, not just string matching |

## What 0.12 builtins handle

| Concern | How |
|---------|-----|
| LSP config | `vim.lsp.config()` + `vim.lsp.enable()` |
| Completion | Built-in autocomplete (`vim.opt.autocomplete = true`) |
| Commenting | Built-in `gc`/`gcc` |
| Format on save | `BufWritePre` autocmd + `vim.lsp.buf.format()` + `MiniTrailspace.trim()` |
| Snippets | Built-in snippet engine |
| Node selection | `v_an` / `v_in` |
| URL open | `gx` |
| Plugin management | `vim.pack.add()` + lockfile |

## Setup

First launch clones all plugins via `vim.pack`. Then install LSP servers:

### macOS (brew + cargo)

```bash
brew install fzf ripgrep fd tree-sitter-cli zoxide tmux lazygit
brew install lua-language-server bash-language-server taplo uv
brew install gopls
brew install typescript-language-server vscode-langservers-extracted
brew install typos-lsp vale rust-analyzer zk
uv tool install ty ruff
cargo install --git https://github.com/errata-ai/vale-ls
```

### Linux (mise)

```bash
# Toolchain (mise provides node/npm and rust/cargo)
mise use node@latest rust@latest fzf@latest ripgrep@latest fd@latest tree-sitter@latest vale@latest zoxide@latest

# LSP servers via mise
mise use github:LuaLS/lua-language-server
mise use github:tekumara/typos-lsp
mise use cargo:taplo-cli
mise use aqua:zk-org/zk
go install golang.org/x/tools/gopls@latest

# LSP servers via npm (needs node above)
npm i -g bash-language-server typescript-language-server typescript
npm i -g vscode-langservers-extracted

# Python LSP servers
uv tool install ty ruff

# LSP servers via cargo (needs rust above)
cargo install --git https://github.com/errata-ai/vale-ls
```

For treesitter parser installs, `nvim-treesitter` also needs the `tree-sitter` CLI in
`PATH` (installed by `fedora/setup-mise.sh` via `tree-sitter@latest` on Fedora/Linux,
and by `brew install tree-sitter-cli` on macOS), plus `tar`, `curl`, and a working C
compiler.

`fzf-lua` also expects `tmux` for `<leader>fB` (tmux paste buffers) and `zoxide` for
`<leader>fz` (recent directories). The `zoxide` picker opens `oil` in the selected
directory instead of changing Neovim's cwd.

Then run `:TSSync` in nvim to install treesitter parsers.

## Commands

| Command | What it does |
|---------|-------------|
| `:PackUpdate` | Update all plugins (review diff, `:w` to confirm) |
| `:LspInfo` | Show LSP clients for current buffer |
| `:TSSync` | Install missing treesitter parsers |
| `:TSUpdate` | Update treesitter parsers |

## Key mappings

See `lua/keymaps/` for the full list (split into `core`, `find`, `git`, `search`, `notes`, `lsp`). Highlights:

| Key | Action |
|-----|--------|
| **Source control (`<leader>g`)** | |
| `<leader>gg` | Lazygit (tab terminal) |
| `<leader>gf` | Git status (fzf) |
| `<leader>gc` | Commits — repo (fzf) |
| `<leader>gh` | History — buffer commits (fzf) |
| `<leader>gb` | Blame (fzf) |
| `<leader>gB` | Branches (fzf) |
| `<leader>gS` | Stash (fzf) |
| `<leader>gT` | Tags (fzf) |
| `<leader>gd` | Git diff |
| `<leader>gU` | Git diff current file (unified) |
| `<leader>gD` | Git diff current file (side by side) |
| `<leader>go` | Diff overlay toggle (mini.diff) |
| `<leader>gl` | Git log with stats |
| `<leader>gL` | Git log current file with stats |
| `<leader>gi` | Git inspect at cursor (mini.git) |
| `<leader>gt` | Code review (tuicr) |
| `<leader>gj` | Jujutsu (jj-fugitive) |
| **Find (`<leader>f`)** | |
| `<leader>f.` | Resume last picker |
| `<leader>ff` | Find files |
| `<leader>fF` | VCS files |
| `<leader>fg` | Live grep (rg) |
| `<leader>f/` | Resume live grep |
| `<leader>fG` | Git grep |
| `<leader>fb` | Buffers |
| `<leader>fB` | Tmux clipboard |
| `<leader>fc` | Commands |
| `<leader>fo` | Recent files |
| `<leader>fh` | Help tags |
| `<leader>fk` | Keymaps |
| `<leader>fj` | Jumps |
| `<leader>fl` | Buffer lines |
| `<leader>fC` | Changes (edit positions) |
| `<leader>fm` | Marks |
| `<leader>f,` | Registers |
| `<leader>fq` | Quickfix |
| `<leader>fQ` | Location list |
| `<leader>fw` | Grep word under cursor |
| `<leader>fz` | Zoxide recent directories (open in Oil) |
| `<leader>fd` | Document diagnostics |
| `<leader>fD` | Workspace diagnostics |
| `<leader>fs` | Document symbols |
| `<leader>fS` | Workspace symbols |
| `<leader>fr` | Search & replace (grep → quickfix) |
| `<leader>fR` | Replace in quickfix files |
| `<leader>ft` | Grep TODO/FIX (cwd) |
| `<leader>fT` | Grep TODO/FIX (project root or buffer dir) |
| `<leader>fv` | Semantic search (vecgrep) |
| `<leader>fV` | Live semantic search |
| `<leader>fX` | Reindex vecgrep |
| **Code (`<leader>c`)** | |
| `<leader>ca` | Code actions |
| `<leader>cr` | Rename |
| `<leader>cf` | Format |
| `<leader>ch` | Toggle inlay hints |
| `<leader>ci` | Incoming calls |
| `<leader>co` | Outgoing calls |
| `<leader>cF` | Finder (defs+refs+impls) |
| **Notes (`<leader>z`)** | |
| `<leader>zf` | Find notes |
| `<leader>zn` | New note |
| `<leader>zN` | New permanent note |
| `<leader>zw` | Weekly journal |
| `<leader>zs` | Search notes |
| `<leader>zz` | Find by tag |
| `<leader>zl` | Linked notes |
| `<leader>zb` | Backlinks |
| **Diagnostics (`<leader>e`)** | |
| `<leader>ee` | Diagnostic float |
| `<leader>el` | Diagnostics to loclist |
| `<leader>em` | Messages history |
| `<leader>en` | Notification history |
| **Markdown Preview (`<leader>p`)** | |
| `<leader>pp` | LaTeX popup (markdown only) |
| **Markdown Tools (`<leader>t`)** | |
| `<leader>tf` | Flash card (markdown only) |
| `<leader>tt` | Toggle todo (markdown only) |
| `<leader>td` | Insert current date (markdown only) |
| `<leader>tr` | Reset timestamp timer (markdown only) |
| `<leader>tv` | Edit Vale accepted words (markdown only) |
| `<leader>tV` | Edit Vale rejected words (markdown only) |
| `<leader>tc` | Disable timestamp counter (markdown only) |
| `<leader>ti` | Insert elapsed timestamp (markdown only) |
| **LSP** | |
| `gd` | Go to definition |
| `gD` | Declaration |
| `gr` | References |
| `gi` | Implementations |
| `gy` | Type definitions |
| `K` | Hover docs |
| **Editor** | |
| `<C-/>` | Toggle terminal |
| `-` | Oil file explorer |
| `ZX` | Save and close buffer |
| `gc`/`gcc` | Comment (builtin) |
| `sa`/`sd`/`sr` | Surround add/delete/replace |
| `gS` | Split/join args |
| `Alt-j`/`Alt-k` | Move lines |
| `[d`/`]d` | Prev/next diagnostic |
| `[b`/`]b` | Prev/next buffer |

---

## Learning Guide

Everything below is stuff you have available but might not know about yet.

### 0.12 Builtins

**Commenting (no plugin needed)**
- `gcc` — toggle comment on current line
- `gc` in visual mode — toggle comment on selection
- `gcap` — comment a paragraph
- `gc}` — comment to end of block

**Completion (`autocomplete = true`)**
- Popup appears as you type (disabled in markdown)
- `<C-n>` / `<C-p>` — next/previous item
- `<C-y>` — accept selected item
- `<C-e>` — dismiss popup
- `<C-x><C-o>` — trigger omni completion manually
- `<C-x><C-f>` — file path completion

**Node selection (treesitter)**
- `van` — select outward (expand to parent node)
- `vin` — select inward (shrink to child node)
- Press `an` repeatedly in visual mode to keep expanding

**URL open**
- `gx` — open URL under cursor in browser

**Built-in LSP mappings (0.12 defaults)**
- `grn` — rename (alternative to `<leader>cr`)
- `gra` — code action (alternative to `<leader>ca`)
- `grr` — references (alternative to `gr`)
- `grt` — type definition
- `grx` — run code lens
- `gO` — document symbols

**Diagnostics**
- `]d` / `[d` — next/prev diagnostic (via mini.bracketed)
- `<leader>ee` — diagnostic float at cursor
- `<leader>el` — send diagnostics to location list

**Vale vocabulary**
- `<leader>tv` — open repo-local accepted words for Vale
- `<leader>tV` — open repo-local rejected words for Vale
- Creates `.vale.ini` if missing
- Creates `.vale/styles/config/vocabularies/Local/accept.txt` if missing
- Also creates empty `reject.txt` alongside it
- Uses the nearest `.vale.ini`, `.git`, `.jj`, or `.hg` root

**Quickfix / location list**
- `:copen` / `:cclose` — open/close quickfix list
- `:lopen` / `:lclose` — open/close location list
- `:cnext` / `:cprev` — next/prev quickfix entry
- `:lnext` / `:lprev` — next/prev location-list entry
- `]q` / `[q` — next/prev quickfix entry (via mini.bracketed)
- `]l` / `[l` — next/prev location-list entry (via mini.bracketed)

**Change list (jump to places you edited)**
- `g;` — jump to older change position
- `g,` — jump to newer change position
- `<leader>fC` — fzf picker for change list
- `:changes` — show full change list

**Other**
- `ZX` — save current buffer if changed, then `:bdelete`
- `ZZ` — write and quit
- `ZQ` — quit without saving
- `:help!` — guess help tag at cursor (DWIM)
- `:iput` — paste with auto-indent
- `:uniq` — deduplicate lines in buffer
- `:wall ++p` — save all, auto-create parent directories
- `:restart` — restart nvim, reattach UI
- `:messages` — see past notifications

### Source control (`<leader>g`)

Three layers working together:

- **lazygit** (`<leader>gg`) — full-screen TUI for staging, committing, rebasing, branch management. Opens in a tab terminal so tmux navigation works normally.
- **fzf-lua git pickers** (`<leader>gf/gc/gh/gb/gB/gS/gT`) — fuzzy searchable status, commits, blame, branches with preview panes.
- **git diff + mini.git / mini.diff** (`<leader>gd/gU/gD/go/gl/gL/gi`) — Git diff views, inline overlay, log with stats, and cursor-context inspection.

**Drill-down workflow:**
1. `<leader>gl` — repo log with stats (pick a commit)
2. `<leader>gi` on a commit hash — opens full commit diff
3. `<leader>gi` inside the diff on a hunk — jumps to source file at that line

Git-specific diff views:

- `<leader>gd` — repo diff in a mini.git split
- `<leader>gU` — current-file unified diff
- `<leader>gD` — current-file side-by-side diff (index vs current buffer) using `fugitive-core`
- `q` inside `<leader>gD` closes the side-by-side review tab and returns to the previous view

Note: `<leader>gd`/`<leader>gl`/`<leader>gi` auto-lcd to the VCS root so mini.git path resolution works correctly. `<leader>gD` is Git-only and uses `mini.git` metadata plus `fugitive-core`'s side-by-side helper.

### fzf-lua

**Search**
- `<leader>ff` — find files (respects .gitignore)
- `<leader>fg` — live grep with ripgrep
- `<leader>fG` — git grep
- `<leader>fw` — grep word under cursor
- `<leader>ft` — grep TODO/FIX in cwd
- `<leader>fT` — grep TODO/FIX from project root (`.git`/`.jj`/`.hg`) or buffer dir
- `<leader>fv` — semantic search (vecgrep)
- `<leader>fV` — live semantic search

**Navigation**
- `<leader>fb` — switch buffers
- `<leader>fo` — recent files
- `<leader>fl` — search current buffer lines
- `<leader>fh` — help tags
- `<leader>f.` — resume last picker

**LSP via fzf-lua**
- `gd` — go to definition (single match jumps, multiple shows picker)
- `gr` — references
- `gi` — implementations
- `gy` — type definitions
- `<leader>ca` — code actions
- `<leader>ci` — incoming calls (who calls this?)
- `<leader>co` — outgoing calls (what does this call?)
- `<leader>cF` — combined finder (defs+refs+impls)
- `<leader>fd` — document diagnostics
- `<leader>fs` — document symbols

**Inside fzf picker**
- `<C-j>` / `<C-k>` — move up/down in results
- `<Enter>` — open file
- `<C-v>` — open in vertical split
- `<C-x>` — open in horizontal split
- `<C-t>` — open in new tab
- `<Tab>` — toggle multi-select
- `<C-g>` — toggle fuzzy / regex mode (useful in grep pickers)
- Type to fuzzy filter results

**`<C-g>` regex toggle is most useful in:**
- `<leader>fg` — live grep: switch from fuzzy to exact regex pattern
- `<leader>fG` — git grep: same
- `<leader>fw` — grep word: refine with regex
- `<leader>ft`/`<leader>fT` — TODO grep: narrow results with regex
- `gr` — references: filter by filename pattern
- `<leader>fd` — diagnostics: filter by message pattern

Less useful in file/buffer pickers (`<leader>ff`, `<leader>fb`) where fuzzy matching is already what you want.

### Oil (file explorer)

- `-` — open parent directory (from any buffer)
- Edit filenames in the buffer to rename
- Delete lines to delete files
- Yank/paste lines to copy/move files
- `:w` to apply changes (confirm prompt)
- `<CR>` — open file/directory
- `<M-h>` — open in horizontal split
- `g.` — toggle hidden files

### jj-fugitive (jujutsu)

- Primary VCS power tool in this setup
- `:J` — open jj log (default)
- `:J status` — jj status view
- `:J diff` — diff current revision
- `:J describe` — edit revision description
- `:J bookmark` — manage bookmarks
- `:J annotate` — blame-style annotation

### mini.nvim Modules

**mini.surround** — add/delete/replace surroundings
- `sa` + motion + char — add surrounding (e.g. `saiw"` wraps word in quotes)
- `sd` + char — delete surrounding (e.g. `sd"` removes quotes)
- `sr` + old + new — replace surrounding (e.g. `sr"'` changes `"` to `'`)
- Works with `()`, `[]`, `{}`, `""`, `''`, `` ` ``, `<>`, and custom patterns

**mini.bracketed** — `[`/`]` navigation
- `[b`/`]b` — prev/next buffer
- `[d`/`]d` — prev/next diagnostic
- `[q`/`]q` — prev/next quickfix entry
- `[l`/`]l` — prev/next location list entry
- `[x`/`]x` — prev/next conflict marker
- `[t`/`]t` — prev/next treesitter node
- `[f`/`]f` — prev/next file in directory
- `[i`/`]i` — prev/next indent change
- Capital letter versions jump to first/last (e.g. `[B` = first buffer)

**mini.move** — move text
- `Alt-j` / `Alt-k` — move line down/up (normal mode)
- `Alt-h` / `Alt-l` — move line left/right (indent)
- Same keys in visual mode move the selection

**mini.splitjoin** — toggle single/multi-line
- `gS` — split arguments to multiple lines, or join back to one line
- Works on function args, tables, arrays, objects

**mini.diff** — git diff in gutter
- Signs appear automatically: `+` added, `~` changed, `-` deleted
- `<leader>go` — toggle full diff overlay (shows old text inline)
- Works with git — in jj repos, shows diffs via colocated git

**mini.clue** — key clue popup
- Press any trigger key (`<leader>`, `g`, `z`, `[`, `]`, `<C-w>`, etc.) and pause
- Shows available continuations with descriptions
- 300ms delay before showing

**mini.notify** — notifications
- `vim.notify()` messages appear as floating text in the corner
- Auto-fade after a few seconds
- `<leader>en` — open notification history
- `:lua MiniNotify.show_history()` — see past notifications

**mini.pairs** — auto-close
- Type `(` → inserts `()`  with cursor between
- Type `"` → inserts `""` with cursor between
- `<BS>` on empty pair deletes both
- Smart: won't double-close if closing char already exists

**mini.cursorword** — word highlighting
- Automatically highlights all visible instances of the word under cursor
- No keymap needed — just move cursor

**mini.trailspace** — trailing whitespace
- Highlights trailing whitespace in red
- `:lua MiniTrailspace.trim()` — manual trim (also trimmed on save via autocmd)

**mini.tabline** — buffer tabs
- Shows open buffers as tabs at the top
- Click to switch (or use `<leader>fb` for fzf buffer picker)
- `[b`/`]b` to cycle (via mini.bracketed)

**mini.statusline** — bottom bar
- Left: mode, git branch, diff stats, diagnostics
- Center: filename
- Right: LSP progress, LSP servers, filetype, line:col

**mini.starter** — start screen
- Shows on `nvim` with no file
- Recent files (global + current dir)
- Quick actions: find files, grep, notes, oil, help, update plugins
- Type to filter items, `<Enter>` to select
- `q` to quit

### Search & replace (fzf-lua + quickfix)

1. `<leader>fr` — live grep to find matches
2. `<Tab>` to multi-select specific matches (or select all)
3. `<Enter>` — sends selected to quickfix list
4. `<leader>fR` — prompts for old/new text, runs `:cfdo %s/old/new/gc` across all quickfix files
5. `c` confirms each replacement, `a` replaces all in file, `q` skips file

### Terminal

- `<C-/>` — toggle terminal (keeps session alive)
- `<Esc><Esc>` — exit terminal insert mode to normal mode
- `<C-h/j/k/l>` — navigate to tmux pane from terminal
- In normal mode: yank, search, scroll the terminal buffer like any file

### Format on save

Runs automatically on every `:w`:
1. LSP format (only if the attached LSP supports formatting)
2. Trim trailing whitespace (via mini.trailspace)
3. Trim trailing blank lines (via mini.trailspace)

Manual format: `<leader>cf`

### Repo-local Vale vocabulary

In markdown buffers, `<leader>tv` opens repo-local Vale accepted words and
`<leader>tV` opens repo-local rejected words.

Root detection checks the nearest of:
- `.git`
- `.jj`
- `.hg`

If needed, the mapping creates:
- `.vale.ini`
- `.vale/styles/config/vocabularies/Local/accept.txt`
- `.vale/styles/config/vocabularies/Local/reject.txt`

Generated `.vale.ini`:
```ini
StylesPath = .vale/styles
MinAlertLevel = suggestion
Vocab = Local

[*.md]
BasedOnStyles = Vale
```

This keeps accepted words per repository instead of global, and `vale_ls` is configured
to root on `.git`, `.jj`, or `.hg`, so it picks up the local vocabulary from the current
repository instead of falling back to a global `~/.vale.ini`.

### Per-project config (`exrc`)

Drop a `.nvim.lua` file in any project root to override settings:
```lua
-- .nvim.lua example
vim.opt_local.shiftwidth = 4
vim.opt_local.tabstop = 4
```
Nvim prompts to trust the file on first load.

### Plugin management

- `:PackUpdate` — update all plugins, shows diff, `:w` to confirm, `:q` to discard
- `:LspInfo` — show attached LSP clients and their state (ready/starting/busy)
- `:TSUpdate` — update treesitter parsers
- `:messages` — see past notifications (or `:lua MiniNotify.show_history()`)
