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
set cursorline
set wrap linebreak nolist
set mouse=a
set display=lastline
set autoread
set hidden
set backspace=indent,eol,start
set visualbell

" status bar
set statusline=%<[%n]%m%r%h%w%{'['.(&fenc!=''?&fenc:&enc).':'.&ff.']'}%y\ %F%=%l,%c%V%8P
let g:hi_insert = 'highlight StatusLine guifg=darkblue guibg=darkyellow gui=none ctermfg=white ctermbg=green cterm=none'

if has('syntax')
augroup InsertHook
autocmd!
autocmd InsertEnter * call s:StatusLine('Enter')
autocmd InsertLeave * call s:StatusLine('Leave')
augroup END
endif

  let s:slhlcmd = ''
function! s:StatusLine(mode)
  if a:mode == 'Enter'
  silent! let s:slhlcmd = 'highlight ' . s:GetHighlight('StatusLine')
  silent exec g:hi_insert
  else
  highlight clear StatusLine
  silent exec s:slhlcmd
  endif
  endfunction

function! s:GetHighlight(hi)
  redir => hl
  exec 'highlight '.a:hi
  redir END
  let hl = substitute(hl, '[\r\n]', '', 'g')
  let hl = substitute(hl, 'xxx', '', '')
  return hl
  endfunction


" keybindings
inoremap <C-c> <Esc>
noremap ; :
noremap : ;
inoremap <C-e> <END>
inoremap <C-a> <HOME>
inoremap <C-h> <Left>
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-l> <Right>
inoremap <C-f> <Left>
inoremap <C-b> <Right>
inoremap <C-d> <Delete>
nnoremap <Space> <PageDown>
