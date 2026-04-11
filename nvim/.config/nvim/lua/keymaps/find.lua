----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local util = require("util")

local function fzf_zoxide()
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
end

----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
return function(map)
	map("n", "<leader>f.", "<cmd>FzfLua resume<cr>", { desc = "Resume last picker" })
	map("n", "<leader>ff", function()
		util.fzf_with_cwd("files", util.buf_dir)
	end, { desc = "Find files" })
	map("n", "<leader>fF", function()
		util.fzf_with_cwd("vcs_files", util.vcs_dir)
	end, { desc = "VCS files" })
	map("n", "<leader>fb", "<cmd>FzfLua buffers<cr>", { desc = "Buffers" })
	map("n", "<leader>fB", "<cmd>FzfLua tmux_buffers<cr>", { desc = "Tmux clipboard" })
	map("n", "<leader>fc", "<cmd>FzfLua commands<cr>", { desc = "Commands" })
	map("n", "<leader>fh", "<cmd>FzfLua help_tags<cr>", { desc = "Help tags" })
	map("n", "<leader>fk", "<cmd>FzfLua keymaps<cr>", { desc = "Keymaps" })
	map("n", "<leader>fj", "<cmd>FzfLua jumps<cr>", { desc = "Jumps" })
	map("n", "<leader>fl", "<cmd>FzfLua blines<cr>", { desc = "Buffer lines" })
	map("n", "<leader>fC", "<cmd>FzfLua changes<cr>", { desc = "Changes" })
	map("n", "<leader>fm", "<cmd>FzfLua marks<cr>", { desc = "Marks" })
	map("n", "<leader>f,", "<cmd>FzfLua registers<cr>", { desc = "Registers" })
	map("n", "<leader>fo", "<cmd>FzfLua oldfiles<cr>", { desc = "Recent files" })
	map("n", "<leader>fq", "<cmd>FzfLua quickfix<cr>", { desc = "Quickfix" })
	map("n", "<leader>fQ", "<cmd>FzfLua loclist<cr>", { desc = "Location list" })
	map("n", "<leader>fz", fzf_zoxide, { desc = "Zoxide" })
	map("n", "<leader>fd", "<cmd>FzfLua diagnostics_document<cr>", { desc = "Document diagnostics" })
	map("n", "<leader>fD", "<cmd>FzfLua diagnostics_workspace<cr>", { desc = "Workspace diagnostics" })
	map("n", "<leader>fs", "<cmd>FzfLua lsp_document_symbols<cr>", { desc = "Document symbols" })
	map("n", "<leader>fS", "<cmd>FzfLua lsp_workspace_symbols<cr>", { desc = "Workspace symbols" })
end
