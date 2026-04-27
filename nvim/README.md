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

The walkthrough previously embedded here now lives in [`../docs/NVIM_LEARNING_GUIDE.html`](../docs/NVIM_LEARNING_GUIDE.html), a single self-contained HTML page with interactive quizzes. Open it in a browser locally for the intended experience.
