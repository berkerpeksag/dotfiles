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
set autochdir

" Open another file without saving the current one first
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
set shortmess=atI

" All operations work with the OS clipboard. No need for \"+, \"*
set clipboard=unnamed

" Allow backspacing over everything in insert mode
set backspace=indent,eol,start

" Tab completion settings
"" Wilcard matches show a list, matching the longest first
set wildmode=list:longest
set wildignore+=.git " Ignore version control repos
set wildignore+=__pycache__
set wildignore+=*.o " Ignore object files

" Visual autocomplete for command menu
set wildmenu

" Custom keybindings
let mapleader = ","

"" Create a new tab like Firefox
nmap <C-t> :tabnew<cr>
imap <C-t> <ESC>:tabnew<cr>

"" Disable classic L-R-T-B keys
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

"" Switch to visual mode
inoremap jj <Esc>

"" Row based j and k
nnoremap k gk
nnoremap j gj

"" Turn off search highlight
nnoremap <leader><space> :nohlsearch<CR>

"" Move to beginning/end of line
nnoremap B ^
nnoremap E $

"" Move the current line to down and up in normal and
"" insert modes
nnoremap <leader>mj :.m+1<CR>==
nnoremap <leader>mk :.m.-2<CR>==
inoremap <leader>mj <Esc>:.m+1<CR>==gi
inoremap <leader>mk <Esc>:. m-2<CR>==gi

"" Create a shortcut to mute highlighting
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" Line wrapping
set wrap
set textwidth=79
set formatoptions=qrn1
set colorcolumn=79

" Indentation
"" Use spaces instead of tabs
set expandtab
"" Number of spaces in tab when editing
set softtabstop=4
"" Sets display width of tabs
set tabstop=4
"" Sets indentation with
set shiftwidth=4

" Statusline
"" File name
set statusline=%<%f\
"" Current dir
set statusline+=\ [%{getcwd()}]

" Enable filetype stuff
filetype plugin indent on

" Auto commands

"" Auto-save
autocmd TextChanged,TextChangedI <buffer> silent write

"" Clear whitespace at the end of lines automatically
autocmd BufWritePre * :%s/\s\+$//e

"" Python
autocmd FileType python let python_highlight_all = 1
autocmd FileType python let python_slow_sync = 1
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType python set completeopt=menu
autocmd FileType python nnoremap <buffer> <silent> <leader>r :w<CR> :exec '!python3' shellescape(@%, 1)<cr>


"" JavaScript
autocmd Filetype javascript setlocal ts=2 sts=2 sw=2

" Automatically set paste mode
" https://coderwall.com/p/if9mda
let &t_SI .= "\<Esc>[?2004h"
let &t_EI .= "\<Esc>[?2004l"

inoremap <special> <expr> <Esc>[200~ XTermPasteBegin()

function! XTermPasteBegin()
  set pastetoggle=<Esc>[201~
  set paste
  return ""
endfunction

function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

map <leader>n :call RenameFile()<cr>
