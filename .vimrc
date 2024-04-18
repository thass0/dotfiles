" Settings inspired by https://github.com/amix/vimrc/blob/master/vimrcs/basic.vim

set history=500
" Ensable filetype plugins
filetype plugin on
filetype indent on

" Automatically read file when they're changed from the outside.
set autoread
au FocusGained,BufEnter * silent! checktime

" User interface

set so=7
let $LANG='en'
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
set wildmenu
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

set ruler
set cmdheight=1
set hid
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set ignorecase
set hlsearch
set incsearch
set lazyredraw
set magic
set showmatch
set mat=2
set noerrorbells
set novisualbell
set t_vb=
set tm=500
" set foldcolumn=1

" User interface

syntax enable
set regexpengine=0
if $COLORTERM == 'gnome-terminal'
    set t_Co=256
endif

set encoding=utf8
set ffs=unix,dos,mac

" File backup stuff

set nobackup
set nowb
set noswapfile

" Editor settings

set number
set expandtab
set smarttab
set shiftwidth=4
set tabstop=4
set lbr
set tw=500
set ai
set si
set wrap
au BufReadPost * if line("'\"") > 2 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
set laststatus=2
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{getcwd()}%h\ \ \ Line:\ %l\ \ Column:\ %c
function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction
" set spell spelllang=en_us

inoremap { {}<Esc>ha
inoremap ( ()<Esc>ha
inoremap [ []<Esc>ha
" inoremap " ""<Esc>ha
" inoremap ' ''<Esc>ha
" inoremap ` ``<Esc>ha

" Plugins

call plug#begin()
Plug 'dense-analysis/ale'
" Plug 'airblade/vim-gitgutter'
call plug#end()
