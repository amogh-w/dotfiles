return {
	{
		'akinsho/bufferline.nvim',
		version = "*",
		dependencies = 'nvim-tree/nvim-web-devicons',
		cond = not vim.g.vscode,
		config = function()
			require("bufferline").setup()
		end,
	}
}
