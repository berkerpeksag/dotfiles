" Turn off compability mode with Vi, we don't need that anymore
set nocompatible


" Activate Pathogen
runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()


" Show line and column number
set ruler

" Show line numbers
set number

" Display the current mode
set showmode

" Highlight the current line
set cursorline

" Highlight search terms
set hlsearch


" Statusline

" File name
set statusline=%<%f\
" Git current branch
set statusline+=%{fugitive#statusline()}
" Current dir
set statusline+=\ [%{getcwd()}]


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

" Syntax formatting for languages
syntax enable
set background=dark


" Nerdtree

" Open a NERDTree automatically when vim starts up
autocmd vimenter * NERDTree

" Map the toggle command :NERDTreeToggle to the F2 key
map <F2> :NERDTreeToggle<CR>

" Ignore list
let NERDTreeIgnore=['\.pyc', '\.pyo', '\.pyd', '\~$', '\.bak', '\.swp$', '\.git', '\.hg']
