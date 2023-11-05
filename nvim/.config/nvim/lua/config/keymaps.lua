-- Keymaps are automatically loaded on the VeryLazy event
-- Default keymaps that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/keymaps.lua
-- Add any additional keymaps here

if vim.g.vscode then
    vim.keymap.set("n", "<C-h>", ":call VSCodeNotify('workbench.action.navigateLeft')<CR>",
        { desc = "VSCode: Go to left window" })
    vim.keymap.set("n", "<C-j>", ":call VSCodeNotify('workbench.action.navigateDown')<CR>",
        { desc = "VSCode: Go to down window" })
    vim.keymap.set("n", "<C-k>", ":call VSCodeNotify('workbench.action.navigateUp')<CR>",
        { desc = "VSCode: Go to up window" })
    vim.keymap.set("n", "<C-l>", ":call VSCodeNotify('workbench.action.navigateRight')<CR>",
        { desc = "VSCode: Go to right window" })
    vim.keymap.set("n", "<leader>", ":call VSCodeNotify('whichkey.show')<CR>", { desc = "VSCode: Show Whichkey" })
end
