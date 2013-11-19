scriptencoding utf-8
syntax on
set nocompatible
set number
set helplang=ja
set fencs=utf-8,euc-jp,iso-2022-jp,cp932 
set autoindent smartindent
set smarttab
set tabstop=2 softtabstop=2 shiftwidth=2
set expandtab
set showmatch matchtime=1
set matchpairs+=<:>
set whichwrap+=h,l,<,>,[,],b,s,~
set nowrap
set ruler
set ruf=%45(%12f%=\ %m%{'['.(&fenc!=''?&fenc:&enc).']'}\ %l-%v\ %p%%\ [%02B]%)
"set statusline=%f:%{substitute(getcwd(),'.*/','','')}\ %m%=%{(&fenc!=''?&fenc:&enc).':'.strpart(&ff,0,1)}\ %l-%v\ %p%%\ %02B
set statusline=%<[%n]%m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%y\ %F%=%l,%c%V%8P
set showcmd
set cmdheight=1
set laststatus=2
set ignorecase
set incsearch
set hlsearch
set relativenumber
highlight LineNr ctermfg=white
highlight ZenkakuSpace cterm=underline ctermfg=blue
set lcs=tab:>.
set list
colorscheme ron
set wrap linebreak nolist
set mouse=a
set display=lastline
set autoread
set hidden
set backspace=indent,eol,start
set visualbell
