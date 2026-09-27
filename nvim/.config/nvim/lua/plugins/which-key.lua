return {
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    init = function()
      vim.o.timeout = true
      vim.o.timeoutlen = 300
    end,
    cond = not vim.g.vscode,
    config = function()
      local wk = require("which-key")
      -- wk.register({
      --   f = {
      --     name = "File",
      --     f = { "<cmd>Telescope find_files<cr>", "Find File" },
      --     r = { "<cmd>Telescope oldfiles<cr>", "Open Recent File" },
      --   },
      --   d = { name = "Delete/Close" },
      --   q = { name = "Quit" },
      --   u = { name = "UI" },
      -- }, { prefix = "<leader>" })
      wk.add({
        { "<leader>d", group = "Delete/Close" },
        { "<leader>f", group = "File" },
        { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find File" },
        { "<leader>fr", "<cmd>Telescope oldfiles<cr>", desc = "Open Recent File" },
        { "<leader>q", group = "Quit" },
        { "<leader>u", group = "UI" },
      })
    end,
  }
}
