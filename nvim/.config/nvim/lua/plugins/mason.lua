return {
  "mason-org/mason.nvim",
  dependencies = {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
  },
  event = "VeryLazy",
  config = function()
    require("mason").setup()

    local ensure_installed = {
      "bash-language-server", -- bashls
      "codelldb",
      "eslint-lsp", -- eslint
      "graphql-language-service-cli", -- graphql
      "jsonlint",
      "lua-language-server", -- lua_ls
      "luacheck",
      "prettier",
      "rust-analyzer", -- rust_analyzer
      "shellcheck",
      "sqlfluff",
      "typescript-language-server", -- ts_ls
      "typos-lsp", -- typos_lsp
      "vale-ls",
      -- handled by zk.lua
      -- "zk",
    }
    require("mason-tool-installer").setup({ ensure_installed = ensure_installed })
  end,
}
