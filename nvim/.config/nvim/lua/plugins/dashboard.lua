return {
    {
        'nvimdev/dashboard-nvim',
        event = 'VimEnter',
        dependencies = { { 'nvim-tree/nvim-web-devicons' } },
        cond = not vim.g.vscode,
        config = function()
            require('dashboard').setup({
                theme = "doom",
                config = {
                    center = {
                        { action = "Telescope find_files", desc = " Find file", icon = " ", key = "f" },
                        { action = "ene | startinsert", desc = " New file", icon = " ", key = "n" },
                        { action = "Telescope oldfiles", desc = " Recent files", icon = " ", key = "r" },
                        { action = "Telescope live_grep", desc = " Find text", icon = " ", key = "g" },
                        { action = "Lazy", desc = " Lazy", icon = "󰒲 ", key = "l" },
                        { action = "qa", desc = " Quit", icon = " ", key = "q" },
                    },
                }
            })
        end,
    }
}
