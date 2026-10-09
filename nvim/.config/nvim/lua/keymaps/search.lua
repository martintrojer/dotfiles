----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local util = require("util")

local map = vim.keymap.set

map("n", "<leader>sg", function()
	util.fzf_with_cwd("live_grep", util.buf_dir)
end, { desc = "Live grep (rg)" })
map("n", "<leader>s/", function()
	require("fzf-lua").live_grep({ resume = true })
end, { desc = "Resume live grep" })
map("n", "<leader>sG", function()
	util.fzf_with_cwd("grep", util.vcs_dir, {
		cmd = "git grep --line-number --color=always",
		prompt = "Git Grep> ",
	})
end, { desc = "Git grep" })
map("n", "<leader>sw", function()
	util.fzf_with_cwd("grep_cword", util.buf_dir)
end, { desc = "Grep word under cursor" })
