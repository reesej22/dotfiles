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
nnoremap <leader>f :Files<CR>
nnoremap <leader>b :Buffers<CR>
nnoremap <leader>c :Commands<CR>

"" Asyncomplete
imap <c-space> <Plug>(asyncomplete_force_refresh)
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr>    pumvisible() ? asyncomplete#close_popup() : "\<cr>"
autocmd! CompleteDone * if pumvisible() == 0 | pclose | endif

"" Emmet & HTML
let g:user_emmet_leader_key=','
autocmd FileType html setlocal tabstop=2 shiftwidth=2 softtabstop=2

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
