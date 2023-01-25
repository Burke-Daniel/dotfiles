let mapleader=","

call plug#begin()

Plug 'tpope/vim-sensible'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'

Plug 'christoomey/vim-tmux-navigator'

Plug 'morhetz/gruvbox'

Plug 'ervandew/supertab'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()

set bg=dark
colorscheme gruvbox

set splitbelow splitright
set number relativenumber
set cursorline cursorcolumn
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4

nnoremap <space> $

nnoremap <leader>ff :Files<cr>
nnoremap <leader>fg :GFiles<cr>
nnoremap <leader>fb :Buffers<cr>
nnoremap <leader>r :source ~/.vimrc<cr>


