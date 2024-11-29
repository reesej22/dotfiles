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

"" Sign Column
set signcolumn="yes"

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
nnoremap <Esc><Esc> :nohlsearch<CR>

"" Colors
set t_Co=256
set background=dark
set termguicolors
colorscheme retrobox

"" Spelling
set spelllang=en_us
set spellsuggest=best

"" Windows Quick Keys
nnoremap <C-h> <C-w>h<CR>
nnoremap <C-j> <C-w>j<CR>
nnoremap <C-k> <C-w>k<CR>
nnoremap <C-l> <C-w>l<CR>
nnoremap <C-q> :bdelete %<CR>
nnoremap <C-t> :belowright terminal<CR>
nnoremap <silent>\ :NERDTreeToggle<CR>

"""""""""""""""""
"  Vim Plugins  "
"""""""""""""""""
"" Vim-Plug
call plug#begin()
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'dense-analysis/ale'
  Plug 'prabirshrestha/vim-lsp'
  Plug 'rhysd/vim-lsp-ale'
  Plug 'prabirshrestha/asyncomplete.vim'
  Plug 'prabirshrestha/asyncomplete-lsp.vim'
  Plug 'prabirshrestha/asyncomplete-emmet.vim'
  Plug 'mattn/vim-lsp-settings'
  Plug 'mattn/emmet-vim'
  Plug 'liuchengxu/vim-which-key'
  Plug 'LunarWatcher/auto-pairs'
  Plug 'tpope/vim-fugitive'
  Plug 'preservim/nerdtree'
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
call plug#end()

"" Vim Airline
let g:airline_theme='angr'
let g:airline#extensions#fzf#enabled = 1
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#lsp#enabled = 1
let g:airline#extensions#nerdtree_statusline = 1

"" Vim Ale
let g:ale_disable_lsp = 'auto'

"" FZF Config
let g:fzf_vim = {}

" Default: Use quickfix list
let g:fzf_vim.listproc = { list -> fzf#vim#listproc#quickfix(list) }
" let g:fzf_vim.listproc = { list -> fzf#vim#listproc#location(list) }

" Command-wise customization
let g:fzf_vim.listproc_ag = { list -> fzf#vim#listproc#quickfix(list) }
" let g:fzf_vim.listproc_rg = { list -> fzf#vim#listproc#location(list) }

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Path completion with custom source command
" inoremap <expr> <c-x><c-f> fzf#vim#complete#path('fdfind')
inoremap <expr> <c-x><c-f> fzf#vim#complete#path('rg --files')

" Word completion with custom spec with popup layout option
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'window': { 'width': 0.2, 'height': 0.9, 'xoffset': 1 }})

" Global line completion (not just open buffers. ripgrep required.)
inoremap <expr> <c-x><c-l> fzf#vim#complete(fzf#wrap({
  \ 'prefix': '^.*$',
  \ 'source': 'rg -n ^ --color always',
  \ 'options': '--ansi --delimiter : --nth 3..',
  \ 'reducer': { lines -> join(split(lines[0], ':\zs')[2:], '') }}))

function! s:make_sentence(lines)
  return substitute(join(a:lines), '^.', '\=toupper(submatch(0))', '').'.'
endfunction

" Word Search
inoremap <expr> <c-x><c-s> fzf#vim#complete({
  \ 'source':  'batcat /usr/share/dict/words',
  \ 'reducer': function('<sid>make_sentence'),
  \ 'options': '--multi --reverse --margin 15%,0',
  \ 'left':    20})

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
  elseif &filetype == 'c' || 'cpp' || 'zig'
    set tabstop=4 shiftwidth=4 expandtab
  elseif &filetype == 'html' || &filetype == 'css' || &filetype == 'javascript'
    set tabstop=2 shiftwidth=2 expandtab
  elseif &filetype == 'vim' || 'lua'
    set tabstop=2 shiftwidth=2 expandtab
  endif
endfunction

"" Use an autocommand to trigger the function
autocmd FileType * call SetFileTypeSettings()

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
let g:which_key_map.s = {
  \ 'name' : '+FZF' ,
  \ 'b' : ['Buffers'               , 'Buffers']          ,
  \ 'C' : ['Changes'               , 'Changes']          ,
  \ 'f' : ['Files'                 , 'Files']            ,
  \ 'g' : ['GFiles'                , 'Git Files']        ,
  \ 'h' : ['History'               , 'History']          ,
  \ 'l' : ['Lines'                 , 'Lines']            ,
  \ 'm' : ['Maps'                  , 'Maps']             ,
  \ 'r' : ['Rg'                    , 'RipGrep']          ,
  \ 's' : ['Colors'                , 'Colorschemes']     ,
  \ 'w' : ['Windows'               , 'FZF Windows']      ,
  \ 'x' : ['Commands'              , 'Commands']         ,
  \ '?' : ['Helptags'              , 'Help Tags']        ,
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
  let g:lsp_format_sync_timeout = 1000
  autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
" refer to doc to add more commands
endfunction

"" Auto Install LSP
augroup lsp_install
  au!
  autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

"" Git Status
function! GitBranch()
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = GitBranch()
  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

""""""""""""""""""""""""
" Auto Folding Config  "
""""""""""""""""""""""""
"" Autofolding .vimrc
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
"augroup fold_vimrc
"  autocmd!
"  autocmd FileType vim
"      \ setlocal foldmethod=expr |
"      \ setlocal foldexpr=VimFolds(v:lnum) |
"      \ setlocal foldtext=VimFoldText() |
"     "\ set foldcolumn=2 foldminlines=2
"augroup END

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
