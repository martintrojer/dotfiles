local function git_cmd(args)
	vim.cmd("Git " .. args)
end

vim.api.nvim_create_user_command("G", function(opts)
	local args = vim.trim(opts.args or "")
	if args == "" then
		git_cmd("diff -- %")
		return
	end
	git_cmd(args)
end, {
	nargs = "*",
	desc = "Git entrypoint, defaulting to diff of current file",
})

vim.api.nvim_create_user_command("Gb", function()
	git_cmd("blame -- %")
end, { desc = "Git blame current file" })

vim.api.nvim_create_user_command("Gl", function()
	git_cmd("log --stat -200 -- %")
end, { desc = "Git log for current file (last 200)" })

vim.api.nvim_create_user_command("Glg", function()
	git_cmd("log --stat -200")
end, { desc = "Git log with stats (last 200)" })

vim.api.nvim_create_user_command("GG", function()
	MiniGit.show_at_cursor()
end, { desc = "Git inspect at cursor" })
