return {
    {
        "nvim-treesitter/nvim-treesitter",
        build = ":TSUpdate",
        config = function()
            local configs = require("nvim-treesitter.configs")
            configs.setup({
                ensure_installed = { "cpp", "python", "lua", "vim" },
                sync_install = false,
                highlight = { enable = true },
                indent = { enable = true },
            })
        end
    }
}
