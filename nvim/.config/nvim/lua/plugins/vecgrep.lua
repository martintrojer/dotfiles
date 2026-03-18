return {
  "martintrojer/vecgrep.nvim",
  dependencies = { "folke/snacks.nvim" },
  event = "VeryLazy",
  opts = {
    search_from_root = true,
  },
  keys = {
    {
      "<leader>zF",
      function()
        vim.cmd("Vecgrep " .. vim.fn.input("Vecgrep: "))
      end,
      desc = "Semantic Search",
    },
    {
      "<leader>zf",
      function()
        local text = table.concat(vim.fn.getregion(vim.fn.getpos("v"), vim.fn.getpos(".")), " ")
        vim.cmd("Vecgrep " .. text)
      end,
      mode = "v",
      desc = "Semantic Search Selection",
    },
    { "<leader>zL", "<Cmd>VecgrepLive<CR>", desc = "Live Semantic Search" },
    { "<leader>zR", "<Cmd>VecgrepReindex<CR>", desc = "Reindex Vecgrep" },
  },
}
