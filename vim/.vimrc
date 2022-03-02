set nocompatible    " Use Vim defaults instead of 100% vi compatibility
set backspace=indent,eol,start  " more powerful backspacing

if has("syntax")
  syntax on
endif

set background=dark

" have Vim load indentation rules and plugins according to the detected filetype.
filetype plugin indent on

set showcmd            " Show (partial) command in status line.
set showmatch          " Show matching brackets.
set ignorecase         " Do case insensitive matching
set smartcase          " Do smart case matching
set incsearch          " Incremental search
"set autowrite          " Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
"set mouse=a            " Enable mouse usage (all modes)

:" Map Ctrl-A -> Start of line, Ctrl-E -> End of line
:noremap <C-a> <Home>
:noremap <C-e> <End>
:inoremap <C-a> <Home>
:inoremap <C-e> <End>
