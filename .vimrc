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
"" Compatibility with vi
set nocompatible

"" Set Leader Keys
let g:mapleader = "\<Space>"
let g:maplocalleader = ','

"" Clipboard
set clipboard=unnamedplus

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

"""""""""""""""""
"  Vim Plugins  "
"""""""""""""""""
"" Ensure that Vundle is installed
function! EnsureVundle()
  " Check if Vundle is installed
  if !isdirectory(expand("~/.vim/bundle/Vundle.vim"))
    echo "Vundle is not installed. Installing now..."
    silent! execute "!git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim"
    if v:shell_error
      echoerr "Failed to clone Vundle. Please check your Git configuration."
    else
      echo "Vundle installed successfully!"
    endif
  endif
endfunction

" Call the function before plugin initialization
call EnsureVundle()

"" Vundle Setup
filetype off                 " Required for Vundle
set rtp+=~/.vim/bundle/Vundle.vim

call vundle#begin()
  Plugin 'VundleVim/Vundle.vim'
  Plugin 'vim-airline/vim-airline'
  Plugin 'vim-airline/vim-airline-themes'
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
  Plugin 'LunarWatcher/auto-pairs'
call vundle#end()

filetype plugin indent on    " Requred for Vundle

"" Syntastic
let g:syntastic_check_on_open = 1
let g:systastic_enble_balloons = 1

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

"" Which-key Settings
nnoremap <silent> <leader>      :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey  ','<CR>
let g:which_key_map = {}
let g:which_key_map.b = {
  \ 'name' : '+buffer' ,
  \ '1' : ['b1'                    , 'buffer 1']         ,
  \ '2' : ['b2'                    , 'buffer 2']         ,
  \ 'd' : ['bd'                    , 'delete-buffer']    ,
  \ 'f' : ['bfirst'                , 'first-buffer']     ,
  \ 'h' : ['Startify'              , 'home-buffer']      ,
  \ 'l' : ['blast'                 , 'last-buffer']      ,
  \ 'n' : ['bnext'                 , 'next-buffer']      ,
  \ 'p' : ['bprevious'             , 'previous-buffer']  ,
  \ '?' : ['Buffers'               , 'fzf-buffer']       ,
  \ }
let g:which_key_map.f = {
  \ 'name' : '+FZF' ,
  \ 'b' : ['Buffers'               , 'FZF Buffers']      ,
  \ 'c' : ['Commands'              , 'FZF Commands']     ,
  \ 'f' : ['Files'                 , 'FZF Files']        ,
  \ 'l' : ['Lines'                 , 'FZF Lines']        ,
  \ 's' : ['Colors'                , 'Colorschemes']     ,
  \ 'w' : ['Windows'               , 'FZF Windows']      ,
  \ }
let g:which_key_map.l = {
  \ 'name' : '+LSP' ,
  \ 'c' : ['LspCodeLens'           , 'Code Lens']        ,
  \ 'd' : ['LspDefinition'         , 'LSP Definition']   ,
  \ 'f' : ['LspDocumentFormat'     , 'Format Document']  ,
  \ 'h' : ['LspHover'              , 'Hover']            ,
  \ 'i' : ['LspInstallServer'      , 'Install Server']   ,
  \ 'm' : ['LspManageServers'      , 'Manage Servers']   ,
  \ 'n' : ['LspNextDiagnostic'     , 'Next Diagnostic']  ,
  \ 'p' : ['LspPreviousDiagnostic' , 'Prev Diagnostic']  ,
  \ 's' : ['LspStatus'             , 'LSP Status']       ,
  \ }
let g:which_key_map.n = {
  \ 'name' : '+NerdTree' ,
  \ 'f' : ['NERDTreeFind'          , 'NERD Find']        ,
  \ 'n' : ['NERDTree'              , 'NERD Tree']        ,
  \ 't' : ['NERDTreeToggle'        , 'NERD Toggle']      ,
  \ }
let g:which_key_map.p = {
  \ 'name' : '+Spelling' ,
  \ 's' : ['set spell'             , 'Spell Check']      ,
  \ 'n' : ['set nospell'           , 'No Spell Check']   ,
  \ 'g' : ['spellgood'             , 'Add Word as Good'] ,
  \ 'r' : ['spellwrong'            , 'Add Word as Wrong'],
  \ 'u' : ['spellundo'             , 'Undo Spellgood']   ,
  \ }
let g:which_key_map.w = {
  \ 'name' : '+windows' ,
  \ 'w' : ['<C-W>w'                , 'Other Window']     ,
  \ 'd' : ['<C-W>c'                , 'Delete Window']    ,
  \ '-' : ['<C-W>s'                , 'Split Below']      ,
  \ '|' : ['<C-W>v'                , 'Split Right']      ,
  \ '2' : ['<C-W>v'                , 'Double Columns']   ,
  \ 'h' : ['<C-W>h'                , 'Focus Left']       ,
  \ 'j' : ['<C-W>j'                , 'Focus Below']      ,
  \ 'l' : ['<C-W>l'                , 'Focus Right']      ,
  \ 'k' : ['<C-W>k'                , 'Focus Up']         ,
  \ 'H' : ['<C-W>5<'               , 'Expand Left']      ,
  \ 'J' : [':resize +5'            , 'Expand Down']      ,
  \ 'L' : ['<C-W>5>'               , 'Expand Right']     ,
  \ 'K' : [':resize -5'            , 'Expand Up']        ,
  \ '=' : ['<C-W>='                , 'Balance Windows']  ,
  \ }
let g:which_key_map.z = {
  \ 'name' : '+Folds' ,
  \ 'a'    : ['za'                 , 'Toggle Fold']      ,
  \ 'c'    : ['zc'                 , 'Close Fold']       ,
  \ 'd'    : ['zd'                 , 'Delete Fold']      ,
  \ 'e'    : ['zE'                 , 'Eliminate Folds']  ,
  \ 'f'    : ['zF'                 , 'Create Fold']      ,
  \ 'm'    : ['zM'                 , 'Foldlevel 0']      ,
  \ 'o'    : ['zo'                 , 'Open Fold']        ,
  \ 'r'    : ['zR'                 , 'Foldlevel Max']    ,
  \ }
call which_key#register('<Space>', "g:which_key_map")

"""""""""""""""""""
"  Vim Functions  "
"""""""""""""""""""
"" Vim-LSP
function! s:on_lsp_buffer_enabled() abort
  setlocal omnifunc=lsp#complete
  setlocal signcolumn=yes
  if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
  nmap <buffer> gd <plug>(lsp-definition)
  nmap <buffer> gs <plug>(lsp-document-symbol-search)
  nmap <buffer> gS <plug>(lsp-workspace-symbol-search)
  nmap <buffer> gr <plug>(lsp-references)
  nmap <buffer> gi <plug>(lsp-implementation)
  nmap <buffer> gt <plug>(lsp-type-definition)
  nmap <buffer> <leader>rn <plug>(lsp-rename)
  nmap <buffer> [g <plug>(lsp-previous-diagnostic)
  nmap <buffer> ]g <plug>(lsp-next-diagnostic)
  nmap <buffer> K <plug>(lsp-hover)
  nnoremap <buffer> <expr><c-f> lsp#scroll(+4)
  nnoremap <buffer> <expr><c-d> lsp#scroll(-4)
  
  let g:lsp_format_sync_timeout = 1000
  autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
" refer to doc to add more commands
endfunction

"" Auto Install LSP
augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

"" Delete trailing white space on save, useful for some filetypes ;)
fun! CleanExtraSpaces()
  let save_cursor = getpos(".")
  let old_query = getreg('/')
  silent! %s/\s\+$//e
  call setpos('.', save_cursor)
  call setreg('/', old_query)
endfun

"" Function to set tab settings based on file type
function! SetFileTypeSettings()
  if &filetype == 'python'
    set tabstop=4 shiftwidth=4 expandtab
  elseif &filetype == 'bash'
    set tabstop=4 shiftwidth=4 noexpandtab
  elseif &filetype == 'c'
    set tabstop=8 shiftwidth=8 noexpandtab
  elseif &filetype == 'html' || &filetype == 'css' || &filetype == 'javascript'
    set tabstop=2 shiftwidth=2 expandtab
  elseif &filetype == 'vim'
    set tabstop=2 shiftwidth=2 expandtab
  endif
endfunction

"" Use an autocommand to trigger the function
autocmd FileType * call SetFileTypeSettings()

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
  set guifont=Hack\ Nerd\ Font\ 10
  colorscheme retrobox
endif
