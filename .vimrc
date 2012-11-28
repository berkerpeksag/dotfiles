" Turn off compability mode with Vi, we don't need that anymore
set nocompatible

set t_Co=256

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

" If you are in foo/bar.py and want to edit foo/baz.py you only have to do
" :e baz.py.
"
" If you then substitute :e with :b it will only complete to files that were
" already open. Because this also supports partial matches, very often a :b
" baz is enough to go to the baz.py file.
set autochdir

" Never ever let Vim write a backup file!
set nobackup
set noswapfile

" Hightlight whitespaces
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Easy window navigation
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

" Line wrapping
set wrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=79


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
" Run the Flake8 check every time you write a Python file
autocmd BufWritePost *.py call Flake8()

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
let NERDTreeIgnore=['\.pyc', '\.pyo', '\.pyd', '\~$', '\.bak', '\.git', '\.hg']
