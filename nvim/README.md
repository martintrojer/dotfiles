# Neovim Config

This is a small Neovim 0.12 configuration built on `vim.pack`, Neovim
built-ins, and mini.nvim. It uses no framework or separate plugin manager.

## Philosophy

- nvim is a rarely used hand editor. Markdown reading and notes are `ramble`,
  diff review is `debrief`; neither gets a nvim counterpart. Prefer stock behaviour over
  new plugins or keymaps.
- Neovim 0.12 built-ins provide LSP, commenting, snippets, node selection, and URL opening.
- fzf-lua handles file search, grep, LSP navigation, buffer switching, and
  `vim.ui.select`.
- One mini.nvim repository provides 12 modules through a consistent API.
- `vim.pack` manages plugins with a lockfile and no bootstrap script.
- The configuration defines its keymaps and plugins explicitly.

## Structure

```
init.lua                        — options, colorscheme, diagnostics, commands, autocommands
lua/
  plugins.lua                   — vim.pack.add + build hooks
  mini_setup.lua                — mini module setup
  keymaps/                      — keymaps split by domain
    init.lua                    — requires each domain module
    core.lua                    — editor, terminal, tmux nav, buffer mgmt
    find.lua                    — fzf-lua pickers
    git.lua                     — source control (fzf git pickers)
    search.lua                  — grep
    lsp.lua                     — LSP navigation (definitions, references, calls)
  lsp.lua                       — LSP server configs + enable
  util.lua                      — picker cwd helpers (buffer dir, VCS root)
  lua_globals.lua               — lua-language-server globals list
after/ftplugin/
  markdown.lua                  — soft-wrap prose
```

## Plugins (6 vim.pack entries)

### mini.nvim (11 modules from one repo)

| Module | Purpose |
|--------|---------|
| mini.bracketed | `[`/`]` navigation (buffers, diagnostics, quickfix) |
| mini.clue | Key clue popup on prefix keys |
| mini.cursorword | Highlight word under cursor |
| mini.hipatterns | Highlight TODO/FIX/HACK/NOTE and hex colors inline |
| mini.icons | File/filetype icons |
| mini.indentscope | Indent scope guide line |
| mini.notify | Floating notifications |
| mini.statusline | Statusline (mode, diagnostics, LSP, position) |
| mini.surround | Add/delete/change surroundings |
| mini.tabline | Buffer tab bar |
| mini.trailspace | Highlight trailing whitespace |

### Other plugins

| Plugin | Purpose | Why not builtin? |
|--------|---------|-----------------|
| catppuccin | Catppuccin Mocha theme | Unified theme across terminal, eza, bat, tmux, waybar |
| fzf-lua | Fuzzy finder + LSP navigation | No builtin picker. Uses fzf binary. Also handles `vim.ui.select` |
| oil.nvim | File explorer as editable buffer | Nothing like it builtin — rename/move/delete by editing text |
| vim-tmux-navigator | Tmux pane navigation | Requires matching tmux config. No builtin tmux awareness |
| nvim-treesitter | Parser management | 0.12 ships treesitter runtime but needs this for parser install/update |

## What 0.12 builtins handle

| Concern | How |
|---------|-----|
| LSP config | `vim.lsp.config()` + `vim.lsp.enable()` |
| Commenting | Built-in `gc`/`gcc` |
| Snippets | Built-in snippet engine |
| Node selection | `v_an` / `v_in` |
| URL open | `gx` |
| Plugin management | `vim.pack.add()` + lockfile |

## Setup

First launch clones all plugins via `vim.pack`. The CLI tools and most LSP
servers (fzf, ripgrep, fd, tree-sitter, zoxide, tmux, lua-language-server,
typos-lsp, rust-analyzer, mdroots, taplo, plus shellcheck for bashls linting)
come from the shared mise manifest; see [`mise/README.md`](../mise/README.md). The rest, on both OSes:

```bash
go install golang.org/x/tools/gopls@latest
npm i -g bash-language-server typescript
npm i -g vscode-langservers-extracted
uv tool install ty ruff
```

For treesitter parser installs, `nvim-treesitter` also needs the `tree-sitter`
CLI in `PATH` (from mise), plus `tar`, `curl`, and a working C compiler.

`fzf-lua` also expects `tmux` for `<leader>fB` (tmux paste buffers) and `zoxide` for
`<leader>fz` (recent directories). The `zoxide` picker opens `oil` in the selected
directory instead of changing Neovim's cwd.

Then run `:TSSync` in nvim to install treesitter parsers.

## Commands

| Command | What it does |
|---------|-------------|
| `:PackUpdate` | `:TSSync` + `:TSUpdate`, then update all plugins (review diff, `:w` to confirm) |
| `:LspInfo` | Show LSP clients (alias for `:checkhealth vim.lsp`) |
| `:TSSync` | Install missing treesitter parsers |
| `:TSUpdate` | Update treesitter parsers |

## Key mappings

See `lua/keymaps/` for the full list (split into `core`, `find`, `git`, `search`, `lsp`). Highlights:

| Key | Action |
|-----|--------|
| **Source control (`<leader>g`)** | |
| `<leader>gf` | Git status (fzf) |
| `<leader>gc` | Commits — repo (fzf) |
| `<leader>gh` | History — buffer commits (fzf) |
| `<leader>gb` | Blame (fzf) |
| **Find (`<leader>f`)** | |
| `<leader>f.` | Resume last picker |
| `<leader>ff` | Find files |
| `<leader>fF` | VCS files |
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
| `<leader>fz` | Zoxide recent directories (open in Oil) |
| `<leader>fd` | Document diagnostics |
| `<leader>fD` | Workspace diagnostics |
| `<leader>fs` | Document symbols |
| `<leader>fS` | Workspace symbols |
| **Search (`<leader>s`)** | |
| `<leader>sg` | Live grep (rg) |
| `<leader>s/` | Resume live grep |
| `<leader>sG` | Git grep |
| `<leader>sw` | Grep word under cursor |
| **Code (`<leader>c`)** | |
| `<leader>ca` | Code actions (typo fixes; mdroots quick fixes, link or extract a visual selection) |
| `<leader>cf` | Format buffer (or selection) |
| `<leader>ch` | Toggle inlay hints |
| `<leader>ci` | Incoming calls |
| `<leader>co` | Outgoing calls |
| `<leader>cF` | Finder (defs+refs+impls) |
| **Diagnostics (`<leader>e`)** | |
| `<leader>ee` | Diagnostic float |
| `<leader>el` | Diagnostics to loclist |
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
| `[d`/`]d` | Prev/next diagnostic |
| `[b`/`]b` | Prev/next buffer |
