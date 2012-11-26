"Turn off compability mode with Vi, we don't need that anymore
set nocompatible

" show line and column number
set ruler

" Show line numbers
set number

" Enable filetype stuff
filetype on
filetype plugin on
filetype plugin indent on

" Python stuff
autocmd FileType python let python_highlight_all = 1
autocmd FileType python let python_slow_sync = 1
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType python set expandtab shiftwidth=4 softtabstop=4
autocmd FileType python set completeopt=menu
" Tabs are converted to spaces
autocmd FileType python set expandtab

" syntax formatting for languages
syntax enable
set background=dark
