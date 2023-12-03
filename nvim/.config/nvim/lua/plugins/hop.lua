return {
    {
        'phaazon/hop.nvim',
        branch = 'v2',
        config = function()
            require('hop').setup()
            require("helpers.keys").map(
                { "n", "v" },
                "<leader>s",
                "<cmd>HopWord<cr>",
                "Go to any word in the current buffer"
            )
        end
    }
}
