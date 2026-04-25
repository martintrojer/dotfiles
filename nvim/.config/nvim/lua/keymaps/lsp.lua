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
			-- Wrap the project-wide `map` so each binding only spells the
			-- buffer scope and desc once. mode defaults to "n".
			local function bmap(lhs, rhs, desc, mode)
				map(mode or "n", lhs, rhs, { buffer = ev.buf, desc = desc })
			end

			bmap("gd", "<cmd>FzfLua lsp_definitions<cr>", "Definition")
			bmap("gD", vim.lsp.buf.declaration, "Declaration")
			bmap("gr", "<cmd>FzfLua lsp_references<cr>", "References")
			bmap("gi", "<cmd>FzfLua lsp_implementations<cr>", "Implementations")
			bmap("gy", "<cmd>FzfLua lsp_typedefs<cr>", "Type Definitions")
			bmap("K", vim.lsp.buf.hover, "Hover")
			bmap("<leader>cr", vim.lsp.buf.rename, "Rename")
			bmap("<leader>ca", "<cmd>FzfLua lsp_code_actions<cr>", "Code Actions")
			bmap("<leader>cf", function()
				vim.lsp.buf.format({ async = true })
			end, "Format", { "n", "v" })
			bmap("<leader>ch", function()
				toggle_inlay_hints(ev.buf)
			end, "Toggle Hints")
			bmap("<leader>ci", "<cmd>FzfLua lsp_incoming_calls<cr>", "Incoming calls")
			bmap("<leader>co", "<cmd>FzfLua lsp_outgoing_calls<cr>", "Outgoing calls")
			bmap("<leader>cF", "<cmd>FzfLua lsp_finder<cr>", "Finder (defs+refs+impls)")
		end,
	})
end
