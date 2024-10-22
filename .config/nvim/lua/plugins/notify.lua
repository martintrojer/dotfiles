return {
	"rcarriga/nvim-notify",
	event = "VeryLazy",
	config = function()
		vim.notify = require("notify")
		vim.keymap.set(
			"n",
			"<leader>so",
			require("telescope").extensions.notify.notify,
			{ desc = "[S]earch n[O]tifications" }
		)
	end,
}
