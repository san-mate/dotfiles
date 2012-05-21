syntax on

" Line Number
set number
nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

filetype plugin indent on

"""""""""""""""""""
""" Tabs and spaces
set autoindent " enable autoindenting
set smartindent " smarter indenting
set shiftwidth=4 " 4 spaces indent
set softtabstop=4 " spaces that represent a tab
set tabstop=4 " replace tabs with 4 spaces
set expandtab " expand tabs to spaces
set smarttab " smarting tab
set backspace=indent,eol,start " backspace behavior

colorscheme delek

set cc=79
