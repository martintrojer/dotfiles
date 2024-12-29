return {
	"robitx/gp.nvim",
	event = "VeryLazy",
	config = function()
		local conf = {}
		require("gp").setup(conf)
	end,
}
