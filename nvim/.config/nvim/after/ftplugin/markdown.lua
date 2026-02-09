-- Some folding malarkey
vim.g.markdown_recommended_style = 0

if vim.fn.exists(":Copilot") == 2 then
  vim.cmd("Copilot disable")
end

-- Insert zk link
vim.keymap.set("i", "[[", "<Cmd>ZkInsertLink<CR>", { buffer = 0, desc = "Insert zk link" })

-- Carddown card
vim.keymap.set("n", "<leader>zc", function()
  local pos = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local nline = line:sub(0, pos) .. "PROMPT : RESPONSE 🧠 #tag" .. line:sub(pos + 1)
  vim.api.nvim_set_current_line(nline)
end, { desc = "Insert flash card" })

-- Date
vim.keymap.set("n", "<leader>zi", function()
  local pos = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local nline = line:sub(0, pos) .. "## " .. os.date("%Y-%m-%d") .. line:sub(pos + 1)
  vim.api.nvim_set_current_line(nline)
end, { desc = "Insert current date" })

----------------------------------
--- Todo
local todo = require("toggletodo")

vim.keymap.set("n", "<leader>zt", function()
  todo.ToggleTodo({ v = true })
end, { desc = "Toggle todo" })

----------------------------------
--- Timestamp
local ts = require("timestamps")

vim.keymap.set("n", "<leader>tz", function()
  ts.reset()
end, { desc = "Reset timestamp timer and counter" })

vim.keymap.set("n", "<leader>td", function()
  ts.disable_counter()
  print("counter disabled")
end, { desc = "Disable timestamp counter" })

vim.keymap.set("n", "<leader>ti", function()
  local fullstr = ts.get_ts_string()
  if fullstr == nil then
    return
  end
  local pos = vim.api.nvim_win_get_cursor(0)[2]
  local line = vim.api.nvim_get_current_line()
  local nline = line:sub(0, pos) .. "## " .. fullstr .. line:sub(pos + 1)
  vim.api.nvim_set_current_line(nline)
end, { desc = "Insert elapsed time (and counter) since timestamp start" })
