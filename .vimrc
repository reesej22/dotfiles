"---------------------------------------------------"
"                                                   "
"       ██╗   ██╗██╗███╗   ███╗██████╗  ██████╗     "
"       ██║   ██║██║████╗ ████║██╔══██╗██╔════╝     "
"       ██║   ██║██║██╔████╔██║██████╔╝██║          "
"       ╚██╗ ██╔╝██║██║╚██╔╝██║██╔══██╗██║          "
"        ╚████╔╝ ██║██║ ╚═╝ ██║██║  ██║╚██████╗     "
"         ╚═══╝  ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝     "
"                                                   "
"---------------------------------------------------"

""""""""""""""""""""""
"  General Settings  "
""""""""""""""""""""""
"" Load Defaults
source $VIMRUNTIME/defaults.vim

"" Compatibility with vi
set nocompatible

"" Clipboard
set clipboard=autoselectplus

"" Backspace
set backspace=indent,eol,nostop

"" Mouse
set mouse=a

"" Syntax
syntax on

"" Text Wrapping
set nowrap

"" Line Numbers
set number
set relativenumber

"" Highlighting
set cursorline
:highlight Cursorline cterm=bold ctermbg=black
set showmatch

"" Menu
set wildmenu

"" Backup Files
set noswapfile
set nobackup

"" File Encoding
set encoding=utf-8

"" Terminal Rendering
set ttyfast

"" Error Bells
set noerrorbells
set vb t_vb=

"" Indentations
set autoindent
set smartindent
set list listchars=tab:»-,lead:·,extends:»,precedes:«

"" Tabs
set tabstop=4
set softtabstop=4
set shiftwidth=4
set smarttab
set expandtab

"" Searching
set magic
set ignorecase
set smartcase
set incsearch
set hlsearch
nnoremap <leader>\ :nohlsearch<CR>

"" Colors
set t_Co=256
set background=dark
set termguicolors
colorscheme retrobox

"" Spelling
set spelllang=en_us
set spellsuggest=best
nnoremap <leader>s :set spell<CR>
nnoremap <leader>S :set nospell<CR>

"""""""""""""""""
"  Vim Plugins  "
"""""""""""""""""
"" Vundle Setup
filetype off                 " Required for Vundle
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
    Plugin 'VundleVim/Vundle.vim'
    Plugin 'tpope/vim-fugitive'
    Plugin 'preservim/nerdtree'
    Plugin 'vim-syntastic/syntastic'
    Plugin 'junegunn/fzf', { 'do' : { -> fzf#install() }}
    Plugin 'junegunn/fzf.vim'
    Plugin 'prabirshrestha/vim-lsp'
    Plugin 'prabirshrestha/asyncomplete.vim'
    Plugin 'prabirshrestha/asyncomplete-lsp.vim'
    Plugin 'prabirshrestha/asyncomplete-emmet.vim'
    Plugin 'mattn/vim-lsp-settings'
    Plugin 'mattn/emmet-vim'
    Plugin 'liuchengxu/vim-which-key'
call vundle#end()

filetype plugin indent on    " Requred for Vundle

"" Vim-Airline Settings
" let g:airline#extensions#tabline#enabled = 1
" let g:airline#extensions#tabline#formatter = 'default'
let g:airline_theme='distinguished'

"" Syntastic
let g:syntastic_check_on_open = 1
let g:systastic_enble_balloons = 1

"" NerdTree
nnoremap <leader>n :NERDTreeFocus<CR>
nnoremap <leader>t :NERDTreeToggle<CR>

"" Fzf
let g:fzf_vim={}

"" Asyncomplete
imap <c-space> <Plug>(asyncomplete_force_refresh)
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

"" Emmet & HTML
let g:user_emmet_leader_key=','
autocmd FileType html setlocal tabstop=2 shiftwidth=2 softtabstop=2

"" Which-key Settings
let g:mapleader = "\<Space>"
let g:maplocalleader = ','
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
let g:which_key_map = {}
let g:which_key_map.b = {
      \ 'name' : '+buffer' ,
      \ '1' : ['b1'             , 'buffer 1']        ,
      \ '2' : ['b2'             , 'buffer 2']        ,
      \ 'd' : ['bd'             , 'delete-buffer']   ,
      \ 'f' : ['bfirst'         , 'first-buffer']    ,
      \ 'h' : ['Startify'       , 'home-buffer']     ,
      \ 'l' : ['blast'          , 'last-buffer']     ,
      \ 'n' : ['bnext'          , 'next-buffer']     ,
      \ 'p' : ['bprevious'      , 'previous-buffer'] ,
      \ '?' : ['Buffers'        , 'fzf-buffer']      ,
      \ }
let g:which_key_map.f = {
      \ 'name' : '+FZF' ,
      \ 'b' : ['Buffers'        , 'FZF Buffers']      ,
      \ 'c' : ['Commands'       , 'FZF Commands']     ,
      \ 'f' : ['Files'          , 'FZF Files']        ,
      \ 'l' : ['Lines'          , 'FZF Lines']        ,
      \ 's' : ['Colors'         , 'Colorschemes']     ,
      \ 'w' : ['Windows'        , 'FZF Windows']      ,
      \ }
let g:which_key_map.n = {
      \ 'name' : '+NerdTree' ,
      \ 'f' : ['NERDTreeFind'   , 'NERD Find']         ,
      \ 'n' : ['NERDTree'       , 'NERD Tree']         ,
      \ 't' : ['NERDTreeToggle' , 'NERD Toggle']       ,
      \ }
let g:which_key_map.w = {
      \ 'name' : '+windows' ,
      \ 'w' : ['<C-W>w'     , 'other-window']          ,
      \ 'd' : ['<C-W>c'     , 'delete-window']         ,
      \ '-' : ['<C-W>s'     , 'split-window-below']    ,
      \ '|' : ['<C-W>v'     , 'split-window-right']    ,
      \ '2' : ['<C-W>v'     , 'layout-double-columns'] ,
      \ 'h' : ['<C-W>h'     , 'window-left']           ,
      \ 'j' : ['<C-W>j'     , 'window-below']          ,
      \ 'l' : ['<C-W>l'     , 'window-right']          ,
      \ 'k' : ['<C-W>k'     , 'window-up']             ,
      \ 'H' : ['<C-W>5<'    , 'expand-window-left']    ,
      \ 'J' : [':resize +5'  , 'expand-window-below']  ,
      \ 'L' : ['<C-W>5>'    , 'expand-window-right']   ,
      \ 'K' : [':resize -5'  , 'expand-window-up']     ,
      \ '=' : ['<C-W>='     , 'balance-window']        ,
      \ 's' : ['<C-W>s'     , 'split-window-below']    ,
      \ 'v' : ['<C-W>v'     , 'split-window-below']    ,
      \ '?' : ['Windows'    , 'fzf-window']            ,
      \ }
call which_key#register('<Space>', "g:which_key_map")

"""""""""""""""""""
"  Vim Functions  "
"""""""""""""""""""
"" Delete trailing white space on save, useful for some filetypes ;)
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun

""""""""""""""""""""""""
" Auto Folding Config  "
""""""""""""""""""""""""
"" Autofolding .vimrc
" see http://vimcasts.org/episodes/writing-a-custom-fold-expression/
""" defines a foldlevel for each line of code
function! VimFolds(lnum)
  let s:thisline = getline(a:lnum)
  if match(s:thisline, '^"" ') >= 0
    return '>2'
  endif
  if match(s:thisline, '^""" ') >= 0
    return '>3'
  endif
  let s:two_following_lines = 0
  if line(a:lnum) + 2 <= line('$')
    let s:line_1_after = getline(a:lnum+1)
    let s:line_2_after = getline(a:lnum+2)
    let s:two_following_lines = 1
  endif
  if !s:two_following_lines
      return '='
  else
    if (match(s:thisline, '^"""""') >= 0) &&
       \ (match(s:line_1_after, '^"  ') >= 0) &&
       \ (match(s:line_2_after, '^""""') >= 0)
      return '>1'
    else
      return '='
    endif
  endif
endfunction

""" defines a foldtext
function! VimFoldText()
  " handle special case of normal comment first
  let s:info = '('.string(v:foldend-v:foldstart).' l)'
  if v:foldlevel == 1
    let s:line = ' ◇ '.getline(v:foldstart+1)[3:-2]
  elseif v:foldlevel == 2
    let s:line = '   ●  '.getline(v:foldstart)[3:]
  elseif v:foldlevel == 3
    let s:line = '     ▪ '.getline(v:foldstart)[4:]
  endif
  if strwidth(s:line) > 80 - len(s:info) - 3
    return s:line[:79-len(s:info)-3+len(s:line)-strwidth(s:line)].'...'.s:info
  else
    return s:line.repeat(' ', 80 - strwidth(s:line) - len(s:info)).s:info
  endif
endfunction

""" set foldsettings automatically for vim files
augroup fold_vimrc
  autocmd!
  autocmd FileType vim
      \ setlocal foldmethod=expr |
      \ setlocal foldexpr=VimFolds(v:lnum) |
      \ setlocal foldtext=VimFoldText() |
     "\ set foldcolumn=2 foldminlines=2
augroup END
nnoremap <leader>r zR<CR>
nnoremap <leader>m zM<CR>

"""""""""""""""""""
"  Gvim Settings  "
"""""""""""""""""""
"" guioptions
if has("gui_running")
    set guioptions+=d
    set guioptions+=a
    set guioptions-=T
    set guioptions-=l
    set guioptions-=b
    set guioptions-=r
    set guifont=Hack\ Nerd\ Font\ 14
    colorscheme retrobox
endif
