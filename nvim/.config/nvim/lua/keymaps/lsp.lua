----------------------------------------------------------------------
-- Public API
----------------------------------------------------------------------
return function(map)
	----------------------------------------------------------------------
	-- Helpers
	----------------------------------------------------------------------
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
	-- Buffer-Local Keymaps
	----------------------------------------------------------------------
	vim.api.nvim_create_autocmd("LspAttach", {
		callback = function(ev)
			local o = { buffer = ev.buf }
			map("n", "gd", "<cmd>FzfLua lsp_definitions<cr>", vim.tbl_extend("force", o, { desc = "Definition" }))
			map("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", o, { desc = "Declaration" }))
			map("n", "gr", "<cmd>FzfLua lsp_references<cr>", vim.tbl_extend("force", o, { desc = "References" }))
			map(
				"n",
				"gi",
				"<cmd>FzfLua lsp_implementations<cr>",
				vim.tbl_extend("force", o, { desc = "Implementations" })
			)
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
			map(
				"n",
				"<leader>ci",
				"<cmd>FzfLua lsp_incoming_calls<cr>",
				vim.tbl_extend("force", o, { desc = "Incoming calls" })
			)
			map(
				"n",
				"<leader>co",
				"<cmd>FzfLua lsp_outgoing_calls<cr>",
				vim.tbl_extend("force", o, { desc = "Outgoing calls" })
			)
			map(
				"n",
				"<leader>cF",
				"<cmd>FzfLua lsp_finder<cr>",
				vim.tbl_extend("force", o, { desc = "Finder (defs+refs+impls)" })
			)
		end,
	})
end
