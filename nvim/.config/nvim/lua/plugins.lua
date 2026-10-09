-- Plugin management via vim.pack

----------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------
local gh = function(x)
	return "https://github.com/" .. x
end

----------------------------------------------------------------------
-- Build Hooks
----------------------------------------------------------------------
vim.api.nvim_create_autocmd("User", {
	pattern = "PackChanged*",
	callback = function(ev)
		local name, kind = ev.data.spec.name, ev.data.kind
		if name == "nvim-treesitter" and (kind == "install" or kind == "update") then
			vim.cmd("TSUpdate")
		end
	end,
})

----------------------------------------------------------------------
-- Plugin Specs
----------------------------------------------------------------------
vim.pack.add({
	{ src = gh("catppuccin/nvim"), name = "catppuccin" },
	{ src = gh("echasnovski/mini.nvim"), name = "mini.nvim" },
	gh("ibhagwan/fzf-lua"),
	gh("stevearc/oil.nvim"),
	gh("christoomey/vim-tmux-navigator"),
	gh("nvim-treesitter/nvim-treesitter"),
})
