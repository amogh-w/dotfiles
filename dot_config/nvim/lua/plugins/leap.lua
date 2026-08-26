return {
    {
        url = "https://codeberg.org/andyg/leap.nvim",
        config = function()
            require("leap").add_default_mappings(true)
        end,
    }
}
