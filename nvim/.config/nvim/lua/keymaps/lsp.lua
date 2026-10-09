local map = vim.keymap.set

----------------------------------------------------------------------
-- Buffer-Local Keymaps
----------------------------------------------------------------------
vim.api.nvim_create_autocmd("LspAttach", {
	callback = function(ev)
		local function bmap(lhs, rhs, desc)
			map("n", lhs, rhs, { buffer = ev.buf, desc = desc })
		end

		bmap("gd", "<cmd>FzfLua lsp_definitions<cr>", "Definition")
		bmap("gD", vim.lsp.buf.declaration, "Declaration")
		bmap("gr", "<cmd>FzfLua lsp_references<cr>", "References")
		bmap("gi", "<cmd>FzfLua lsp_implementations<cr>", "Implementations")
		bmap("gy", "<cmd>FzfLua lsp_typedefs<cr>", "Type Definitions")
		bmap("K", vim.lsp.buf.hover, "Hover")
		bmap("<leader>ci", "<cmd>FzfLua lsp_incoming_calls<cr>", "Incoming calls")
		bmap("<leader>co", "<cmd>FzfLua lsp_outgoing_calls<cr>", "Outgoing calls")
		bmap("<leader>cF", "<cmd>FzfLua lsp_finder<cr>", "Finder (defs+refs+impls)")
	end,
})
