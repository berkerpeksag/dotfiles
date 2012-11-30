" Turn off compability mode with Vi, we don't need that anymore
set nocompatible

" Terminal color settings
set t_Co=256


" Activate Pathogen
runtime bundle/pathogen/autoload/pathogen.vim
call pathogen#infect()


" Default to UTF-8 encoding
set encoding=utf8
set fileencoding=utf8


" Syntax formatting for languages
syntax enable
set background=dark

" Color scheme
colorscheme molokai


" Keep track of last commands
set history=1000

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

" Always show the status line
set laststatus=2

" Hightlight whitespaces
set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

" Search settings
set smartcase
set ignorecase
set incsearch
set hlsearch

" Custom keybindings
"" Create a new tab like Firefox
nmap <C-t> :tabnew<cr>
imap <C-t> <ESC>:tabnew<cr>

"" Disable classic L-R-T-B keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>

"" Switch to visual mode
inoremap jj <Esc>

"" Row based j and k
map k gk
map j gj


" Line wrapping
set wrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=79


" Statusline
"" File name
set statusline=%<%f\
"" Git current branch
set statusline+=%{fugitive#statusline()}
"" Current dir
set statusline+=\ [%{getcwd()}]


" Enable filetype stuff
filetype on
filetype plugin on
filetype plugin indent on


" Python
autocmd FileType python let python_highlight_all = 1
autocmd FileType python let python_slow_sync = 1
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType python set expandtab shiftwidth=4 softtabstop=4
autocmd FileType python set completeopt=menu
"" Run the Flake8 check every time you write a Python file
autocmd BufWritePost *.py call Flake8()
"" Tabs are converted to spaces
autocmd FileType python set expandtab

" Nerdtree
"" Open a NERDTree automatically when vim starts up
"autocmd vimenter * NERDTree

"" Map the toggle command :NERDTreeToggle to the F2 key
map <F2> :NERDTreeToggle<CR>

" Close Vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"" Ignore list
let NERDTreeIgnore=['\.pyc', '\.pyo', '\.pyd', '\~$', '\.bak', '\.git', '\.hg']


" Powerline
let g:Powerline_symbols = 'fancy'
