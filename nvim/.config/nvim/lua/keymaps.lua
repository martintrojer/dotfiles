-- Key mappings
local map = vim.keymap.set
local history = require("history")

local function toggle_inlay_hints(bufnr)
	for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr })) do
		if client:supports_method("textDocument/inlayHint") then
			local enabled = vim.lsp.inlay_hint.is_enabled({ bufnr = bufnr })
			vim.lsp.inlay_hint.enable(not enabled, { bufnr = bufnr })
			return
		end
	end
end

----------------------------------------------------------------------
-- General
----------------------------------------------------------------------
map("n", "<esc>", "<cmd>nohlsearch<cr>", { desc = "Clear search" })
map("n", "ZX", "<cmd>update | bdelete<cr>", { desc = "Save and close buffer" })

----------------------------------------------------------------------
-- Diagnostics (mini.bracketed handles [d/]d navigation)
----------------------------------------------------------------------
map("n", "<leader>ee", vim.diagnostic.open_float, { desc = "Diagnostic float" })
map("n", "<leader>el", vim.diagnostic.setloclist, { desc = "Diagnostics to loclist" })
map("n", "<leader>em", history.show_messages, { desc = "Messages history" })
map("n", "<leader>en", history.show_notifications, { desc = "Notification history" })
map("n", "<leader>u", "<cmd>Undotree<cr>", { desc = "Undotree" })

----------------------------------------------------------------------
-- Oil
----------------------------------------------------------------------
map("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })

----------------------------------------------------------------------
-- Terminal toggle: keeps session alive, show/hide with same key
----------------------------------------------------------------------
local term_buf = nil
local term_win = nil
local function toggle_terminal()
	if term_win and vim.api.nvim_win_is_valid(term_win) then
		vim.api.nvim_win_hide(term_win)
		term_win = nil
		return
	end
	if term_buf and vim.api.nvim_buf_is_valid(term_buf) then
		vim.cmd("botright split")
		term_win = vim.api.nvim_get_current_win()
		vim.api.nvim_win_set_buf(term_win, term_buf)
		vim.cmd("startinsert")
	else
		vim.cmd("botright split | term")
		term_buf = vim.api.nvim_get_current_buf()
		term_win = vim.api.nvim_get_current_win()
	end
	vim.api.nvim_win_set_height(term_win, 15)
end
map({ "n", "t" }, "<c-/>", toggle_terminal, { desc = "Toggle terminal" })
map({ "n", "t" }, "<c-_>", toggle_terminal, { desc = "Toggle terminal" })
map("t", "<esc><esc>", "<c-\\><c-n>", { desc = "Exit terminal mode" })

----------------------------------------------------------------------
-- Tmux navigation
----------------------------------------------------------------------
local function tmux_terminal_map(lhs, command, desc)
	map("t", lhs, function()
		vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes("<C-\\><C-N>", true, false, true), "n", false)
		vim.schedule(function()
			vim.cmd(command)
		end)
	end, { desc = desc })
end

tmux_terminal_map("<c-h>", "TmuxNavigateLeft", "Tmux left")
tmux_terminal_map("<c-j>", "TmuxNavigateDown", "Tmux down")
tmux_terminal_map("<c-k>", "TmuxNavigateUp", "Tmux up")
tmux_terminal_map("<c-l>", "TmuxNavigateRight", "Tmux right")

map("n", "<c-h>", "<cmd>TmuxNavigateLeft<cr>", { desc = "Tmux left" })
map("n", "<c-j>", "<cmd>TmuxNavigateDown<cr>", { desc = "Tmux down" })
map("n", "<c-k>", "<cmd>TmuxNavigateUp<cr>", { desc = "Tmux up" })
map("n", "<c-l>", "<cmd>TmuxNavigateRight<cr>", { desc = "Tmux right" })

----------------------------------------------------------------------
-- fzf-lua
----------------------------------------------------------------------
map("n", "<leader>f.", "<cmd>FzfLua resume<cr>", { desc = "Resume last picker" })
map("n", "<leader>fl", "<cmd>FzfLua blines<cr>", { desc = "Buffer lines" })
map("n", "<leader>fC", "<cmd>FzfLua changes<cr>", { desc = "Changes" })
map("n", "<leader>ff", "<cmd>FzfLua files<cr>", { desc = "Find files" })
map("n", "<leader>fF", "<cmd>FzfLua vcs_files<cr>", { desc = "VCS files" })
map("n", "<leader>fg", "<cmd>FzfLua live_grep<cr>", { desc = "Live grep (rg)" })
map("n", "<leader>f/", function()
	require("fzf-lua").live_grep({ resume = true })
end, { desc = "Resume live grep" })
map("n", "<leader>fG", function()
	require("fzf-lua").grep({ cmd = "git grep --line-number --color=always", prompt = "Git Grep> " })
end, { desc = "Git grep" })
map("n", "<leader>fb", "<cmd>FzfLua buffers<cr>", { desc = "Buffers" })
map("n", "<leader>fB", "<cmd>FzfLua tmux_buffers<cr>", { desc = "Tmux clipboard" })
map("n", "<leader>fc", "<cmd>FzfLua commands<cr>", { desc = "Commands" })
map("n", "<leader>fh", "<cmd>FzfLua help_tags<cr>", { desc = "Help tags" })
map("n", "<leader>fk", "<cmd>FzfLua keymaps<cr>", { desc = "Keymaps" })
map("n", "<leader>fj", "<cmd>FzfLua jumps<cr>", { desc = "Jumps" })
map("n", "<leader>fm", "<cmd>FzfLua marks<cr>", { desc = "Marks" })
map("n", "<leader>f,", "<cmd>FzfLua registers<cr>", { desc = "Registers" })
map("n", "<leader>fo", "<cmd>FzfLua oldfiles<cr>", { desc = "Recent files" })
map("n", "<leader>fq", "<cmd>FzfLua quickfix<cr>", { desc = "Quickfix" })
map("n", "<leader>fQ", "<cmd>FzfLua loclist<cr>", { desc = "Location list" })
map("n", "<leader>fw", "<cmd>FzfLua grep_cword<cr>", { desc = "Grep word under cursor" })
map("n", "<leader>fz", function()
	require("fzf-lua").zoxide({
		cmd = "zoxide query --list",
		header = "folder",
		formatter = "path.filename_first",
		path_shorten = 1,
		preview = vim.fn.executable("eza") == 1 and "eza --color=always --icons --group-directories-first {1}"
			or "ls {1}",
		fzf_opts = {
			["--no-multi"] = true,
			["--tiebreak"] = "end,index",
			["--no-sort"] = true,
		},
		actions = {
			enter = function(selected)
				require("oil").open(selected[1]:match("[^\t]+$") or selected[1])
			end,
		},
	})
end, { desc = "Zoxide" })
map("n", "<leader>fd", "<cmd>FzfLua diagnostics_document<cr>", { desc = "Document diagnostics" })
map("n", "<leader>fD", "<cmd>FzfLua diagnostics_workspace<cr>", { desc = "Workspace diagnostics" })
map("n", "<leader>fs", "<cmd>FzfLua lsp_document_symbols<cr>", { desc = "Document symbols" })
map("n", "<leader>fS", "<cmd>FzfLua lsp_workspace_symbols<cr>", { desc = "Workspace symbols" })

-- TODO/FIX grep (shared opts in lua/grep-todos.lua)
local grep_todos = require("grep-todos")
map("n", "<leader>ft", function()
	grep_todos.grep()
end, { desc = "TODOs/FIXes (cwd)" })
map("n", "<leader>fT", function()
	local path = vim.api.nvim_buf_get_name(0)
	local buffer_dir = path ~= "" and vim.fs.dirname(path) or vim.fn.getcwd()
	grep_todos.grep({ cwd = vim.fs.root(0, { ".git", ".jj", ".hg" }) or buffer_dir })
end, { desc = "TODOs/FIXes (repo root)" })

-- Search & replace: grep → <Tab> multi-select → quickfix → cfdo
map("n", "<leader>fr", function()
	require("fzf-lua").live_grep({ prompt = "Search (Tab select → Enter → cfdo)> " })
end, { desc = "Search & replace (grep → quickfix)" })
map("n", "<leader>fR", function()
	vim.ui.input({ prompt = "Replace: " }, function(old)
		if not old or old == "" then
			return
		end
		vim.ui.input({ prompt = "With: " }, function(new)
			if not new then
				return
			end
			vim.cmd("cfdo %s/" .. vim.fn.escape(old, "/") .. "/" .. vim.fn.escape(new, "/") .. "/gc")
		end)
	end)
end, { desc = "Replace in quickfix files" })

----------------------------------------------------------------------
-- Zk notes (all commands pass notebook_path from vim.g.notes_path)
----------------------------------------------------------------------
local np = function()
	return vim.g.notes_path
end
map("n", "<leader>zn", function()
	vim.ui.input({ prompt = "Title: " }, function(title)
		if title then
			vim.cmd("ZkNew { notebook_path = '" .. np() .. "', title = '" .. title .. "', group = 'inbox' }")
		end
	end)
end, { desc = "New Note" })
map("n", "<leader>zN", function()
	vim.ui.input({ prompt = "Title: " }, function(title)
		if title then
			vim.cmd("ZkNew { notebook_path = '" .. np() .. "', title = '" .. title .. "' }")
		end
	end)
end, { desc = "New Permanent Note" })
map("n", "<leader>zw", function()
	vim.cmd("ZkNew { notebook_path = '" .. np() .. "', group = 'journal' }")
end, { desc = "Weekly Journal" })
map("n", "<leader>zf", function()
	vim.cmd("ZkNotes { notebook_path = '" .. np() .. "', sort = { 'modified' } }")
end, { desc = "Find Notes" })
map("n", "<leader>zz", function()
	vim.cmd("ZkTags { notebook_path = '" .. np() .. "' }")
end, { desc = "Find by Tag" })
map("n", "<leader>zs", function()
	vim.ui.input({ prompt = "Search: " }, function(query)
		if query then
			vim.cmd(
				"ZkNotes { notebook_path = '" .. np() .. "', sort = { 'modified' }, match = { '" .. query .. "' } }"
			)
		end
	end)
end, { desc = "Search Notes" })
map("n", "<leader>zl", function()
	vim.cmd("ZkLinks { notebook_path = '" .. np() .. "' }")
end, { desc = "Linked Notes" })
map("n", "<leader>zb", function()
	vim.cmd("ZkBacklinks { notebook_path = '" .. np() .. "' }")
end, { desc = "Backlinks" })

----------------------------------------------------------------------
-- Vecgrep (semantic search alongside fzf-lua text search)
----------------------------------------------------------------------
map("n", "<leader>fv", function()
	vim.ui.input({ prompt = "Vecgrep: " }, function(query)
		if query then
			vim.cmd("Vecgrep " .. query)
		end
	end)
end, { desc = "Semantic search" })
map("v", "<leader>fv", function()
	local text = table.concat(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos(".")), " ")
	vim.cmd("Vecgrep " .. text)
end, { desc = "Semantic search selection" })
map("n", "<leader>fV", "<Cmd>VecgrepLive<CR>", { desc = "Live semantic search" })
map("n", "<leader>fX", "<Cmd>VecgrepReindex<CR>", { desc = "Reindex vecgrep" })

----------------------------------------------------------------------
-- LSP (per-buffer, on attach)
----------------------------------------------------------------------
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(ev)
		local o = { buffer = ev.buf }
		map("n", "gd", "<cmd>FzfLua lsp_definitions<cr>", vim.tbl_extend("force", o, { desc = "Definition" }))
		map("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", o, { desc = "Declaration" }))
		map("n", "gr", "<cmd>FzfLua lsp_references<cr>", vim.tbl_extend("force", o, { desc = "References" }))
		map("n", "gi", "<cmd>FzfLua lsp_implementations<cr>", vim.tbl_extend("force", o, { desc = "Implementations" }))
		map("n", "gy", "<cmd>FzfLua lsp_typedefs<cr>", vim.tbl_extend("force", o, { desc = "Type Definitions" }))
		map("n", "K", vim.lsp.buf.hover, vim.tbl_extend("force", o, { desc = "Hover" }))
		map("n", "<leader>cr", vim.lsp.buf.rename, vim.tbl_extend("force", o, { desc = "Rename" }))
		map(
			"n",
			"<leader>ca",
			"<cmd>FzfLua lsp_code_actions<cr>",
			vim.tbl_extend("force", o, { desc = "Code Actions" })
		)
		map({ "n", "v" }, "<leader>cf", function()
			vim.lsp.buf.format({ async = true })
		end, vim.tbl_extend("force", o, { desc = "Format" }))
		map("n", "<leader>ch", function()
			toggle_inlay_hints(ev.buf)
		end, vim.tbl_extend("force", o, { desc = "Toggle Hints" }))
		map("n", "<leader>ci", "<cmd>FzfLua lsp_incoming_calls<cr>", vim.tbl_extend("force", o, { desc = "Incoming calls" }))
		map("n", "<leader>co", "<cmd>FzfLua lsp_outgoing_calls<cr>", vim.tbl_extend("force", o, { desc = "Outgoing calls" }))
		map("n", "<leader>cF", "<cmd>FzfLua lsp_finder<cr>", vim.tbl_extend("force", o, { desc = "Finder (defs+refs+impls)" }))
	end,
})
