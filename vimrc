" Global settings for all files (but may be overridden in ftplugin).
" set relativenumber " Seems to cause extreme scroll lag in Vim 8
set autoindent
set autoread
set backspace=indent,eol,start
set clipboard=unnamedplus " system copy-paste support 1. get vim-gtk 2. set this 3. for WSL, vcxsrv
set complete-=i
set display+=lastline
set expandtab             " On pressing tab, insert 4 spaces
set formatoptions+=j      " Delete comment character when joining commented lines
set ignorecase
set incsearch
set laststatus=2
set mouse=nic             " Add mouse support for scrolling
set nrformats-=octal
set number
set ruler
set sessionoptions-=options
set shiftwidth=4          " when indenting with '>', use 2 spaces width
set showcmd
set smartcase
set smarttab
set splitright
set tabstop=4             " show existing tab with 4 spaces width
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

colorscheme ron

" lervag/vimtex options
let g:tex_flavor = 'latex'
let g:vimtex_view_method = 'zathura'
let g:vimtex_view_forward_search_on_start = 1
let g:vimtex_quickfix_mode = 0
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
call matchadd('ColorColumn', '\%102v', 100)

" Airline
set encoding=utf-8
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#buffer_idx_mode = 1

" Syntastic
let g:syntastic_enable_racket_racket_checker = 1
let g:syntastic_mode_map = { 'mode': 'passive', 'active_filetypes': [],'passive_filetypes': [] }
nnoremap <C-w>e :SyntasticCheck<CR>
let g:syntastic_tex_lacheck_quiet_messages = { 'regex': '\Vpossible unwanted space at' }

"""""""""""""""""""""""""
" luochen1990/rainbow setup
let g:rainbow_active = 1
let g:rainbow_conf = {
\	'separately': {
\		'*': 0,
\		'racket': {},
\	}
\}
"""""""""""""""""""""""""
" guns/vim-sexp setup
let g:sexp_filetypes = 'clojure,scheme,lisp,timl,racket'
"""""""""""""""""""""""""

" Define <Leader> to be <space>
let mapleader = " "
let g:mapleader = " "

" Fast editing of the .vimrc and other things
nnoremap <leader>e           :e! ~/.vimrc<CR>
nnoremap <leader>u           :e! ~/.vim/UltiSnips/tex.snippets<CR>
nnoremap <leader>h           :set hlsearch!<CR>
nnoremap <leader>p           :set paste!<CR>
nnoremap <leader>s           :SyntasticCheck<CR>
nnoremap <leader>t           :NERDTreeToggle<CR>
" Um, nothing suspicious here (has already converted to Church of Emacs)
nnoremap <silent> <C-x><C-s> :update<CR>
nnoremap <C-c>               <C-c>
nnoremap <silent> <C-x><C-c> :xa<CR>
nnoremap <silent> <C-x>1     :only<CR>
" <CR> will select current selection from pop-up menu Ctrl-N search
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<CR>"
" Insert lambda with <C-\>
inoremap <C-\> λ

if &diff
    highlight DiffAdd    cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
    highlight DiffDelete cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
    highlight DiffChange cterm=bold ctermfg=10 ctermbg=17 gui=none guifg=bg guibg=Red
    highlight DiffText   cterm=bold ctermfg=10 ctermbg=88 gui=none guifg=bg guibg=Red
endif

au BufReadPost dircolors set syntax=dircolors

" Persistent undo
set undofile
set undodir=$HOME/.vim/undo-history

set undolevels=1000
set undoreload=10000

" vim-gutentags
" Config from here: reddit.com/r/vim/comments/d77t6j
let g:gutentags_add_default_project_roots = 0 
let g:gutentags_project_root = ['*.cpp', '.git']
command! -nargs=0 GutentagsClearCache call system('rm ' . g:gutentags_cache_dir . '/*')
let g:gutentags_cache_dir = expand('~/.cache/vim/ctags/')
let g:gutentags_generate_on_new = 1
let g:gutentags_generate_on_missing = 1
let g:gutentags_generate_on_write = 1
let g:gutentags_generate_on_empty_buffer = 0
let g:gutentags_ctags_extra_args = ['--tag-relative=yes', '--fields=+ailmnS',]
let g:gutentags_ctags_exclude = ['.git', '.svg', '.hg', '/tests/', 'build', 'dist',
                    \ 'sites//files/', 'bin', 'node_modules', 'bower_components', 'cache',
                    \ 'compiled', 'docs', 'example', 'bundle', 'vendor', '.md',
                    \ '-lock.json', '.lock', 'bundle.js', 'build.js', '.rc', '.json',
                    \ '.min.', '.map', '.bak', '.zip', '.pyc', '.class', '.sln',
                    \ '.Master', '.csproj', '.tmp', '.csproj.user', '.cache', '.pdb',
                    \ 'tags', 'cscope.', '.css', '.less', '.scss', '.exe', '.dll',
                    \ '.mp3', '.ogg', '.flac', '.swp', '.swo', '.bmp', '.gif', '.ico',
                    \ '.jpg', '.png', '.rar', '.zip', '.tar', '.tar.gz', '.tar.xz', '.tar.bz2',
                    \ '.pdf', '.doc', '.docx', '.ppt', '.pptx', '.tex',]
