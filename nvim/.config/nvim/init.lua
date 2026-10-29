-- Minimal nvim 0.12 config: vim.pack, mini.nvim, fzf-lua
-- See README.md for full documentation

-- Options
vim.g.markdown_recommended_style = 0 -- must be set before built-in ftplugin loads
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
for _, p in ipairs({ "node", "perl", "python3", "ruby" }) do
	vim.g["loaded_" .. p .. "_provider"] = 0
end
vim.g.tmux_navigator_no_mappings = 1

vim.opt.number = true
vim.opt.relativenumber = true
vim.opt.signcolumn = "yes"
vim.opt.clipboard = "unnamedplus"
vim.opt.undofile = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.opt.expandtab = true
vim.opt.shiftwidth = 2
vim.opt.tabstop = 2
vim.opt.scrolloff = 8
vim.opt.cursorline = true
vim.opt.mouse = "a"
vim.opt.winborder = "rounded"
vim.opt.pumborder = "rounded"
vim.opt.exrc = true
vim.opt.autoread = true

-- Plugins + colorscheme
require("plugins")
vim.cmd.packadd("nvim.undotree")
require("catppuccin").setup({
	flavour = "mocha",
	-- `style` replaces catppuccin's own underline/italic on a group;
	-- a bare `bold = true` would merge with it instead.
	custom_highlights = function(c)
		local curl = function(sp)
			return { sp = sp, style = { "undercurl" } }
		end
		return {
			MiniCursorword = { bg = c.surface1, style = {} },
			MiniCursorwordCurrent = { bg = c.surface1, style = {} },
			DiagnosticUnderlineError = curl(c.red),
			DiagnosticUnderlineWarn = curl(c.yellow),
			DiagnosticUnderlineInfo = curl(c.blue),
			DiagnosticUnderlineHint = curl(c.teal),
			DiagnosticUnderlineOk = curl(c.green),
			MiniHipatternsTodo = { fg = c.base, bg = c.yellow, bold = true },
			MiniHipatternsFixme = { fg = c.base, bg = c.overlay1, bold = true },
			MiniHipatternsHack = { fg = c.base, bg = c.overlay0, bold = true },
			MiniHipatternsNote = { fg = c.base, bg = c.overlay0, bold = true },
			MiniIndentscopeSymbol = { fg = c.surface1 },
			MiniTablineFill = { fg = c.overlay0, bg = c.mantle },
			MiniTablineCurrent = { fg = c.base, bg = c.blue, style = { "bold" } },
			MiniTablineVisible = { fg = c.text, bg = c.surface0, bold = true },
			MiniTablineHidden = { fg = c.overlay1, bg = c.mantle },
			MiniTablineModifiedCurrent = { fg = c.base, bg = c.peach, style = { "bold" } },
			MiniTablineModifiedVisible = { fg = c.base, bg = c.teal, bold = true },
			MiniTablineModifiedHidden = { fg = c.peach, bg = c.mantle, bold = true },
			MiniTablineTabpagesection = { fg = c.crust, bg = c.lavender, bold = true },
		}
	end,
})
vim.cmd.colorscheme("catppuccin")

-- Plugin setup
require("mini_setup")

require("fzf-lua").setup({
	"default-title",
	ui_select = {},
	-- tmux grabs <C-q> globally (bind -n C-q clear-history), so move
	-- fzf-lua's send-to-quickfix action off ctrl-q onto alt-q.
	-- `[1] = true` keeps fzf-lua's default file/buffer binds (notably the
	-- `enter` open action); without it this table *replaces* the defaults
	-- and Enter stops opening the selection.
	actions = {
		files = {
			true,
			["alt-q"] = require("fzf-lua.actions").file_sel_to_qf,
		},
	},
})

require("oil").setup({
	default_file_explorer = true,
	columns = { "icon", "permissions", "size", "mtime" },
	keymaps = { ["<C-h>"] = false, ["<M-h>"] = "actions.select_split" },
	view_options = { show_hidden = true },
})

-- Treesitter (run :TSSync to install missing parsers).
-- c, lua, markdown, markdown_inline, query, vim, vimdoc ship with nvim itself.
local ts_parsers = {
	"bash",
	"go",
	"haskell",
	"html",
	"javascript",
	"json",
	"python",
	"rust",
	"toml",
	"tsx",
	"typescript",
	"yaml",
}
vim.api.nvim_create_user_command("TSSync", function()
	if vim.fn.executable("tree-sitter") ~= 1 then
		vim.notify("tree-sitter CLI not found in PATH", vim.log.levels.ERROR)
		return
	end
	require("nvim-treesitter").install(ts_parsers, { summary = true }):wait(300000)
end, { desc = "Install missing treesitter parsers" })

-- Diagnostics
vim.diagnostic.config({
	virtual_text = { spacing = 4, prefix = "●" },
	signs = true,
	underline = true,
	float = { border = "rounded" },
})

-- LSP + keymaps
require("lsp")
require("keymaps")

-- Commands
-- Muscle memory from lspconfig; 0.12 ships the same report as a health check.
vim.api.nvim_create_user_command("LspInfo", "checkhealth vim.lsp", { desc = "LSP status (checkhealth vim.lsp)" })

vim.api.nvim_create_user_command("PackUpdate", function()
	vim.cmd("TSSync")
	vim.cmd("TSUpdate")
	vim.pack.update()
end, { desc = "Update plugins and treesitter parsers" })

-- Autocommands
vim.api.nvim_create_autocmd("FileType", {
	pattern = "nvim-undotree",
	callback = function(ev)
		vim.keymap.set("n", "q", function()
			vim.api.nvim_win_close(0, true)
		end, { buffer = ev.buf, desc = "Close" })
	end,
})

-- Auto-close terminal buffers so they don't block :q
vim.api.nvim_create_autocmd("QuitPre", {
	callback = function()
		local terms, others = {}, 0
		for _, w in ipairs(vim.api.nvim_list_wins()) do
			if vim.bo[vim.api.nvim_win_get_buf(w)].buftype == "terminal" then
				terms[#terms + 1] = vim.api.nvim_win_get_buf(w)
			else
				others = others + 1
			end
		end
		if others <= 1 then
			for _, b in ipairs(terms) do
				vim.api.nvim_buf_delete(b, { force = true })
			end
		end
	end,
})

-- Briefly highlight yanked text.
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.hl.on_yank()
	end,
})

-- Keep `ramble --follow` previews on the cursor in markdown buffers.
require("ramble_follow")

-- Reload buffers changed outside Neovim when refocusing or entering them.
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
	callback = function()
		if vim.bo.modified or not vim.bo.modifiable then
			return
		end
		vim.cmd("checktime")
	end,
})
