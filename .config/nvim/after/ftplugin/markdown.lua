-- Call insert link automatically when we start typing a link
vim.keymap.set("i", "[[", "<cmd>Telekasten insert_link<CR>", { buffer = 0 })

vim.g.markdown_recommended_style = 0
