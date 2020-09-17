" Global settings for all files (but may be overridden in ftplugin).
" set relativenumber " Seems to cause extreme scroll lag in Vim 8
set autoindent
set autoread
set backspace=indent,eol,start
set clipboard=unnamedplus " system copy-paste support 1. get vim-gtk 2. set this 3. for WSL, vcxsrv
set complete-=i
set display+=lastline
set expandtab " On pressing tab, insert 4 spaces
set formatoptions+=j " Delete comment character when joining commented lines
set ignorecase
set incsearch
set laststatus=2
set mouse=nic          " Add mouse support for scrolling
set nrformats-=octal
set number
set ruler
set sessionoptions-=options
set shiftwidth=4 " when indenting with '>', use 4 spaces width
set smartcase
set smarttab
set splitright
set tabstop=4 " show existing tab with 4 spaces width
set viewoptions-=options
set wildmenu

" let g:pathogen_disabled = []
" call add(g:pathogen_disabled, 'tex-conceal.vim')
" call add(g:pathogen_disabled, 'ultisnips')
" let g:pathogen_blacklist = [ 'tex-conceal' ]
execute pathogen#infect()
silent! helptags ALL

" My custom mappings
noremap <C-J> <C-W>j
noremap <C-K> <C-W>k
noremap <C-H> <C-W>h
noremap <C-L> <C-W>l

inoremap <expr> <TAB> pumvisible() ? "\<C-y>" : "\<TAB>"
inoremap <expr> <Esc> pumvisible() ? "\<C-e>" : "\<Esc>"

if exists('g:loaded_sensible') || &compatible
  finish
else
  let g:loaded_sensible = 'yes'
endif

if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

if !has('nvim') && &ttimeoutlen == -1
  set ttimeout
  set ttimeoutlen=100
endif

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif

if &encoding ==# 'latin1' && has('gui_running')
  set encoding=utf-8
endif

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif


if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

if &shell =~# 'fish$' && (v:version < 704 || v:version == 704 && !has('patch276'))
  set shell=/usr/bin/env\ bash
endif

if &history < 1000
  set history=1000
endif
if &tabpagemax < 50
  set tabpagemax=50
endif
if !empty(&viminfo)
  set viminfo^=!
endif

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^Eterm'
  set t_Co=16
endif

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

if empty(mapcheck('<C-U>', 'i'))
  inoremap <C-U> <C-G>u<C-U>
endif
if empty(mapcheck('<C-W>', 'i'))
  inoremap <C-W> <C-G>u<C-W>
endif

colo ron

" lervag/vimtex options
let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
let g:vimtex_view_forward_search_on_start = 1
let g:vimtex_quickfix_mode = 1
autocmd User VimtexEventQuit VimtexClean

" sirver/ultisnips options
let g:UltiSnipsExpandTrigger = '<tab>'
let g:UltiSnipsJumpForwardTrigger = '<tab>'
let g:UltiSnipsJumpBackwardTrigger = '<s-tab>'

" keitaNakamura/tex-conceal.vim options
set conceallevel=1
let g:tex_conceal='abdmg'
hi Conceal ctermbg=none

" Line Highlighting
highlight ColorColumn ctermbg=magenta
call matchadd('ColorColumn', '\%81v', 100)

" Airline
let g:airline#extensions#tabline#enabled = 1
set encoding=utf-8
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1

" Define map leader
let mapleader = " "
let g:mapleader = " "

" Fast editing of the .vimrc
map <leader>e :e! ~/.vimrc<cr>

if &diff
    highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
    highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
    highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
    highlight DiffText   cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red
endif
