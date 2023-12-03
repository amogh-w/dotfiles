return {
	{
		"nvim-neo-tree/neo-tree.nvim",
		branch = "v3.x",
		dependencies = {
			"nvim-lua/plenary.nvim",
			"nvim-tree/nvim-web-devicons",
			"MunifTanjim/nui.nvim",
			-- "3rd/image.nvim", -- Optional image support in preview window: See `# Preview Mode` for more information
		},
		cond = not vim.g.vscode,
		config = function()
			require("neo-tree").setup()
			require("helpers.keys").map(
				{ "n", "v" },
				"<leader>e",
				"<cmd>Neotree toggle<cr>",
				"Toggle file explorer"
			)
		end,
	},
}
