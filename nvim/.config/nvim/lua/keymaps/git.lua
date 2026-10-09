----------------------------------------------------------------------
-- Module References
----------------------------------------------------------------------
local util = require("util")

local map = vim.keymap.set

map("n", "<leader>gf", function()
	util.fzf_with_cwd("git_status", util.vcs_dir)
end, { desc = "Git status" })
map("n", "<leader>gc", function()
	util.fzf_with_cwd("git_commits", util.vcs_dir)
end, { desc = "Git commits (repo)" })
map("n", "<leader>gh", function()
	util.fzf_with_cwd("git_bcommits", util.vcs_dir)
end, { desc = "Git history (buffer)" })
map("n", "<leader>gb", function()
	util.fzf_with_cwd("git_blame", util.vcs_dir)
end, { desc = "Git blame" })
