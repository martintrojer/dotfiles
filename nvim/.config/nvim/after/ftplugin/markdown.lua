vim.g.markdown_recommended_style = 0

if vim.fn.exists(":Copilot") == 2 then
	vim.cmd("Copilot disable")
end

-- Helpers for inserting stuff into buffers
vim.keymap.set("n", "<leader>zc", function()
	local pos = vim.api.nvim_win_get_cursor(0)[2]
	local line = vim.api.nvim_get_current_line()
	local nline = line:sub(0, pos) .. "PROMPT : RESPONSE 🧠 #tag" .. line:sub(pos + 1)
	vim.api.nvim_set_current_line(nline)
end, { desc = "Insert flash card" })

vim.keymap.set("n", "<leader>zi", function()
	local pos = vim.api.nvim_win_get_cursor(0)[2]
	local line = vim.api.nvim_get_current_line()
	local nline = line:sub(0, pos) .. "## " .. os.date("%Y-%m-%d") .. line:sub(pos + 1)
	vim.api.nvim_set_current_line(nline)
end, { desc = "Insert current date" })
