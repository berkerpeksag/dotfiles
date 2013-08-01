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


" Set to auto read when a file is changed from the outside
set autoread

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

" Manage multiple buffers effectively:
"
" 1) The current buffer can be put to the background without writing to disk;
" 2) When a background buffer becomes current again, marks and undo-history are
"    remembered.
set hidden

" Never ever let Vim write a backup file!
set nobackup
set nowritebackup
set noswapfile

" Enable undo even if the file is closed
set undofile
set undodir=/tmp

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

" Set the terminal title
"
" This gives e.g. | spam.py (~) - VIM |.
set title
set titleold=""
set titlestring=Vim:\ %F

" Short prompt. No more |Press ENTER or type command to continue|.
"
" See for more info :help shortmess
set shortmess=atI

" All operations work with the OS clipboard. No need for \"+, \"*
set clipboard=unnamed

" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Tab completion settings
"" Wilcard matches show a list, matching the longest first
set wildmode=list:longest
set wildignore+=.git,.hg,.svn " Ignore version control repos
set wildignore+=*.pyc " Ignore Python compiled files
set wildignore+=*.swp " Ignore vim backups
set wildignore+=*.o " Ignore object files


" Custom keybindings
let mapleader = ","
let g:mapleader = ","

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


" Indentation
"" Use spaces instead of tabs
set expandtab
"" Sets display width of tabs
set tabstop=4
"" Sets indentation with
set shiftwidth=4
"" Turns on auto-indenting
set autoindent
"" Remembers previous indent when creating new lines
set smartindent


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


" Auto commands
"" Clear whitespace at the end of lines automatically
autocmd BufWritePre * :%s/\s\+$//e

"" Python
autocmd FileType python let python_highlight_all = 1
autocmd FileType python let python_slow_sync = 1
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType python set completeopt=menu
""" Run the Flake8 check every time you write a Python file
autocmd BufWritePost *.py call Flake8()

" HTML
"" Convert indentation to 2 spaces in HTML files
autocmd BufNewFile,BufReadPost *.html set shiftwidth=2


" Nerdtree
"" Open a NERDTree automatically when vim starts up
"autocmd vimenter * NERDTree

"" Map the toggle command :NERDTreeToggle to the F2 key
map <F2> :NERDTreeToggle<CR>

" Close Vim if the only window left open is a NERDTree
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

"" Ignore list
let NERDTreeIgnore=['\.pyc', '\.pyo', '\.pyd', '\~$', '\.bak', '\.git', '\.hg', '__pycache__', 'venv']


" Powerline
let g:Powerline_symbols = 'fancy'


" Gist
"" Detect filetype from the filename
let g:gist_detect_filetype = 1

"" Open browser after the post
let g:gist_open_browser_after_post = 1

"" Private by default
let g:gist_post_private = 1
