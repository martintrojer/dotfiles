local map = vim.keymap.set

-- Buffer-Local Keymaps
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
		bmap("<leader>ca", "<cmd>FzfLua lsp_code_actions<cr>", "Code actions")
		map({ "n", "x" }, "<leader>cf", function()
			vim.lsp.buf.format({ async = true })
		end, { buffer = ev.buf, desc = "Format" })
		bmap("<leader>ch", function()
			local f = { bufnr = ev.buf }
			vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled(f), f)
		end, "Toggle inlay hints")
		bmap("<leader>ci", "<cmd>FzfLua lsp_incoming_calls<cr>", "Incoming calls")
		bmap("<leader>co", "<cmd>FzfLua lsp_outgoing_calls<cr>", "Outgoing calls")
		bmap("<leader>cF", "<cmd>FzfLua lsp_finder<cr>", "Finder (defs+refs+impls)")
	end,
})
