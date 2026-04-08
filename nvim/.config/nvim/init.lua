-- Minimal nvim 0.12 config: vim.pack, mini.nvim, fzf-lua
-- See README.md for full documentation

----------------------------------------------------------------------
-- Options
----------------------------------------------------------------------
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"
vim.g.notes_path = vim.fn.expand("~/notes")
vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0
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
vim.opt.softtabstop = 2
vim.opt.scrolloff = 8
vim.opt.cursorline = true
vim.opt.mouse = "a"
vim.opt.winborder = "rounded"
vim.opt.pumborder = "rounded"
vim.opt.autocomplete = true
vim.opt.completeopt:append("nearest")
vim.opt.exrc = true
vim.opt.autoread = true

----------------------------------------------------------------------
-- Plugins + colorscheme
----------------------------------------------------------------------
require("plugins")
vim.cmd.packadd("nvim.difftool")
vim.cmd.packadd("nvim.undotree")
require("catppuccin").setup({ flavour = "mocha" })
vim.cmd.colorscheme("catppuccin")

----------------------------------------------------------------------
-- Plugin setup
----------------------------------------------------------------------
require("mini-setup")
require("starter")

require("fzf-lua").setup({ "default-title", ui_select = true })

require("oil").setup({
	default_file_explorer = true,
	columns = { "icon", "permissions", "size", "mtime" },
	keymaps = { ["<C-h>"] = false, ["<M-h>"] = "actions.select_split" },
	view_options = { show_hidden = true },
})

require("render-markdown").setup({ html = { enabled = false } })
require("jj-fugitive").setup({ default_command = "log", open_mode = "tab", ignore_immutable = true })
require("redline").setup({ providers = { difftool = true, minigit = true } })
require("zk").setup({ picker = "fzf_lua" })
require("vecgrep").setup({ search_from_root = true })

----------------------------------------------------------------------
-- Treesitter (run :TSSync to install missing parsers)
----------------------------------------------------------------------
local ts_parsers = {
	"bash",
	"c",
	"go",
	"haskell",
	"html",
	"javascript",
	"json",
	"latex",
	"lua",
	"markdown",
	"markdown_inline",
	"python",
	"query",
	"rust",
	"toml",
	"tsx",
	"typescript",
	"yaml",
}
vim.api.nvim_create_user_command("TSSync", function()
	local ok, treesitter = pcall(require, "nvim-treesitter")
	if not ok then
		vim.notify("nvim-treesitter is not available", vim.log.levels.ERROR)
		return
	end
	if vim.fn.executable("tree-sitter") ~= 1 then
		vim.notify("tree-sitter CLI not found in PATH", vim.log.levels.ERROR)
		return
	end
	local missing = vim.tbl_filter(function(lang)
		return #vim.api.nvim_get_runtime_file("parser/" .. lang .. ".so", false) == 0
	end, ts_parsers)
	if #missing == 0 then
		vim.notify("TSSync: all " .. #ts_parsers .. " parsers installed", vim.log.levels.INFO)
		return
	end
	for i, lang in ipairs(missing) do
		vim.notify(string.format("TSSync: [%d/%d] installing %s...", i, #missing, lang), vim.log.levels.INFO)
		vim.cmd("redraw")
		treesitter.install({ lang }):wait(300000)
	end
	vim.notify("TSSync: done (" .. #missing .. " parsers installed)", vim.log.levels.INFO)
end, { desc = "Install missing treesitter parsers" })

----------------------------------------------------------------------
-- Diagnostics
----------------------------------------------------------------------
vim.diagnostic.config({
	virtual_text = { spacing = 4, prefix = "●" },
	signs = true,
	underline = true,
	float = { border = "rounded" },
})

----------------------------------------------------------------------
-- LSP + keymaps
----------------------------------------------------------------------
require("lsp")
require("git-commands")
require("keymaps")

----------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------
vim.api.nvim_create_user_command("LspInfo", function()
	local clients = vim.lsp.get_clients({ bufnr = 0 })
	if #clients == 0 then
		print("No LSP clients attached")
		return
	end
	for _, c in ipairs(clients) do
		local state = c.initialized and "ready" or "starting"
		if c.initialized then
			local status = vim.lsp.status()
			if status and status ~= "" and status:find(c.name) then
				state = "busy"
			end
		end
		print(string.format("%s [%s] (id=%d, root=%s)", c.name, state, c.id, c.root_dir or "none"))
	end
end, { desc = "Show LSP clients for current buffer" })

vim.api.nvim_create_user_command("PackUpdate", function()
	vim.pack.update()
end, { desc = "Update all plugins" })

----------------------------------------------------------------------
-- Autocommands
----------------------------------------------------------------------
-- Disable completion in markdown buffers.
vim.api.nvim_create_autocmd("FileType", {
	pattern = "markdown",
	callback = function()
		vim.opt_local.autocomplete = false
	end,
})

vim.api.nvim_create_autocmd("FileType", {
	pattern = "nvim-undotree",
	callback = function(ev)
		vim.keymap.set("n", "q", function()
			vim.api.nvim_win_close(0, true)
		end, { buffer = ev.buf, desc = "Close Undotree" })
	end,
})

-- Enable inlay hints for servers that support them.
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(ev)
		local client = vim.lsp.get_client_by_id(ev.data.client_id)
		if not client or not client:supports_method("textDocument/inlayHint") then
			return
		end
		vim.lsp.inlay_hint.enable(true, { bufnr = ev.buf })
	end,
})

-- Briefly highlight yanked text.
vim.api.nvim_create_autocmd("TextYankPost", {
	callback = function()
		vim.hl.on_yank()
	end,
})

-- Reload buffers changed outside Neovim when refocusing or entering them.
vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
	callback = function()
		if vim.bo.modified or not vim.bo.modifiable then
			return
		end
		vim.cmd("checktime")
	end,
})

-- Format on save and trim trailing whitespace.
vim.api.nvim_create_autocmd("BufWritePre", {
	callback = function()
		if not vim.bo.modifiable then
			return
		end
		local bufnr = vim.api.nvim_get_current_buf()
		local clients = vim.lsp.get_clients({ bufnr = bufnr })
		for _, c in ipairs(clients) do
			if c:supports_method("textDocument/formatting") then
				vim.lsp.buf.format({ bufnr = bufnr, timeout_ms = 500 })
				break
			end
		end
		MiniTrailspace.trim()
		MiniTrailspace.trim_last_lines()
	end,
})
