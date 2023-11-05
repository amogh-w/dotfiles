call plug#begin()
Plug 'tpope/vim-sensible'
Plug 'rebelot/kanagawa.nvim'
call plug#end()

" colorscheme
colorscheme kanagawa

if exists('g:vscode')
    " VSCode extension
    " Better Navigation
    nnoremap <silent> <C-j> :call VSCodeNotify('workbench.action.navigateDown')<CR>
    nnoremap <silent> <C-k> :call VSCodeNotify('workbench.action.navigateUp')<CR>
    nnoremap <silent> <C-h> :call VSCodeNotify('workbench.action.navigateLeft')<CR>
    nnoremap <silent> <C-l> :call VSCodeNotify('workbench.action.navigateRight')<CR>
    nnoremap <silent> <Space> :call VSCodeNotify('whichkey.show')<CR>
else
    " ordinary Neovim
endif
