return {
  {
    "neovim/nvim-lspconfig",
    opts = {
      servers = {
        -- Marksman clashes with zk in unfortunate ways
        marksman = false,
        typos_lsp = {
          filetypes = { "markdown" },
        },
        vale_ls = {
          filetypes = { "markdown" },
        },
      },
    },
  },
}
