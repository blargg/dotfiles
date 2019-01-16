" vimrc
" vim: set ts=3 sw=3 et nowrap:

if has('multi_byte')      " Make sure we have unicode support
   scriptencoding utf-8    " This file is in UTF-8

   " ---- Terminal Setup ----
   if ($ANSWERBACK !=# "PuTTY")
      if (&termencoding == "" && (&term =~ "xterm" || &term =~ "putty")) || (&term =~ "rxvt-unicode") || (&term =~ "screen")
         set termencoding=utf-8
      endif
   endif
   set encoding=utf-8      " Default encoding should always be UTF-8
   let g:airline_powerline_fonts=1
endif

set nocompatible
filetype off               " turn on later
set rtp+=~/.vim/bundle/Vundle.vim/
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'

" General Plugins
Plugin 'autozimu/LanguageClient-neovim', {'branch': 'next', 'do': './install.sh'}
Plugin 'blindFS/vim-taskwarrior'
Plugin 'chrisbra/Recover.vim'
Plugin 'chrisbra/SudoEdit.vim'
Plugin 'ervandew/supertab'
Plugin 'scrooloose/nerdtree'
Plugin 'tomtom/tcomment_vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'
Plugin 'tpope/vim-unimpaired'
Plugin 'w0rp/ale'

" Themes
Plugin 'tomasr/molokai'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'vim-scripts/wombat256.vim'
Plugin 'vim-scripts/Wombat'

" Vim plugins
Plugin 'LnL7/vim-nix'
Plugin 'Shougo/vimproc'

" haskell plugins
" Plugin 'eagletmt/ghcmod-vim'
Plugin 'neovimhaskell/haskell-vim'

" fzf
Plugin 'junegunn/fzf',{'dir':'~/.fzf','do':'./install --all'}
Plugin 'junegunn/fzf.vim'

call vundle#end()

" ---- General Setup ----
set ttyfast
set tabstop=4              " 4 spaces for tabs
set shiftwidth=4           " 4 spaces for indents
set smarttab               " Tab next line based on current line
set expandtab              " Spaces for indentation
set autoindent             " Automatically indent next line
if has('smartindent')
   set smartindent            " Indent next line based on current line
endif
set nowrap
set linebreak              " Display long lines wrapped at word boundaries
set incsearch              " Enable incremental searching
set hlsearch               " Highlight search matches
set ignorecase             " Ignore case when searching...
set smartcase              " ...except when we don't want it
set infercase              " Attempt to figure out the correct case
set showfulltag            " Show full tags when doing completion
set virtualedit=block      " Only allow virtual editing in block mode
set lazyredraw             " Lazy Redraw (faster macro execution)
set foldmethod=syntax      " Fold based on syntax
set foldlevel=10           " Don't start folding until it gets deep
set wildmenu               " Menu on completion please
set wildmode=longest:full,full  " Match the longest substring, complete with first
set wildignore=*.o,*~      " Ignore temp files in wildmenu
set scrolloff=5            " Show lines of context during scrolls
set sidescrolloff=2        " Show 2 columns of context during scrolls
set backspace=2            " Normal backspace behavior
set textwidth=80           " Break lines at 80 characters
set hidden                 " Allow flipping of buffers without saving
set noerrorbells           " Disable error bells
set visualbell             " Turn visual bell on
set t_vb=                  " Make the visual bell emit nothing
set showcmd                " Show the current command
set secure                 " Be safe when using modeline and files
set diffopt+=vertical
set nomodeline             " Disable modeline
set completeopt=menu,menuone,longest
"set exrc                   " Load the .vimrc in the current folder too

let mapleader=" "    " Set leader to space

if v:version > 703
   set formatoptions+=j
endif


" ---- Filetypes ----
syntax on
filetype on             " Detect filetype by extension
filetype indent on      " Enable indents based on extensions
filetype plugin on      " Load filetype plugins

" Open file to last spot
if has('autocmd')
   if has('viminfo')
      autocmd BufReadPost *
         \ if line("'\"") > 0 && line("'\"") <= line("$") |
         \   exe "normal g`\"" |
         \ endif
   endif
endif

" ---- Spelling ----
if (v:version >= 700)
   set spelllang=en_us        " US English Spelling please

   " Toggle spellchecking with F10
   nmap <silent> <F10> :silent set spell!<CR>
   imap <silent> <F10> <C-O>:silent set spell!<CR>
endif

" Display a pretty statusline if we can
if has('title')
   set title
endif
set laststatus=2
set shortmess=atI
if has('statusline')
   set statusline=%{fugitive#statusline()}%<%F\ %r[%{&ff}]%y%m\ %=\ Line\ %l\/%L\ Col:\ %v\ (%P)
endif

" Set colorscheme
if has("gui_running")
   colorscheme molokai
elseif &t_Co == 256
   colorscheme molokai
elseif &t_Co == 88
   colorscheme wombat
else
   colorscheme wombat
endif

" Show trailing whitespace visually
if (&termencoding == "utf-8") || has("gui_running")
   if v:version >= 700
      set list listchars=tab:»·,trail:·,extends:…,nbsp:‗
   else
      set list listchars=tab:»·,trail:·,extends:…
   endif
else
   if v:version >= 700
      set list listchars=tab:>-,trail:.,extends:>,nbsp:_
   else
      set list listchars=tab:>-,trail:.,extends:>
   endif
endif

if has('mouse')
   " Dont copy the listchars when copying
   set mouse=nvi
endif

if has('autocmd')
   " always refresh syntax from the start
   autocmd BufEnter * syntax sync fromstart
endif

" ---- Key Mappings ----
"
" Better window split navigation
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

nmap <c-t> :NERDTreeToggle<CR>

" Clear search colors
nmap <silent> <leader>n :silent :nohlsearch<CR>

" shifted arrows are stupid
inoremap <S-Up> <C-O>gk
noremap  <S-Up> gk
inoremap <S-Down> <C-O>gj
noremap  <S-Down> gj

" Disable q and Q
map q <Nop>
map Q <Nop>

" Toggle numbers with F12
nmap <silent> <F12> :silent set number!<CR>
imap <silent> <F12> <C-O>:silent set number!<CR>

if (&term =~ "^xterm")
   map  <C-[>[H <Home>
   map! <C-[>[H <Home>
   map  <C-[>[F <End>
   map! <C-[>[F <End>
   map  <C-[>[5D <C-Left>
   map! <C-[>[5D <C-Left>
   map  <C-[>[5C <C-Right>
   map! <C-[>[5C <C-Right>
endif

set t_RV=

" Toggle colorcolumn
function! g:ToggleColorColumn()
    if &colorcolumn != ''
        setlocal colorcolumn&
    else
        setlocal colorcolumn=+1
    endif
endfunction

nnoremap <silent> <F2> :call g:ToggleColorColumn() <CR>
inoremap <silent> <F2> <C-O>:call g:ToggleColorColumn() <CR>

" airline
let g:airline_theme='murmur'
let g:airline#extensions#tabline#enabled = 1

" ale
let g:ale_linters = {
   \ 'haskell': ['ghc-mod', 'hlint', 'hie', 'hdevtools']
   \}
let g:ale_haskell_hie_executable = 'hie-wrapper'

" fzf settings
nmap <c-p> :Files<CR>
nmap <leader>p :Files<CR>
nmap <leader>g :GitFiles<CR>
nmap <leader>. :Tags<CR>

" Mapping Search
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

imap <c-x><c-j> <plug>(fzf-complete-file-ag)


let g:syntastic_haskell_checkers = ['ghc_mod', 'hlint']
let g:syntastic_always_populate_loc_list = 1

" haskell fast-tags
augroup tags
   au BufWritePost *.hs       silent! !fast-tags %
   au BufWritePost *.hsc      silent! !fast-tags %
augroup END

" LanguageClient
nnoremap <F5> :call LanguageClient_contextMenu()<CR>
map <Leader>lk :call LanguageClient#textDocument_hover()<CR>
map <Leader>lg :call LanguageClient#textDocument_definition()<CR>
map <Leader>lr :call LanguageClient#textDocument_rename()<CR>
map <Leader>lf :call LanguageClient#textDocument_formatting()<CR>
map <Leader>lb :call LanguageClient#textDocument_references()<CR>
map <Leader>la :call LanguageClient#textDocument_codeAction()<CR>
map <Leader>ls :call LanguageClient#textDocument_documentSymbol()<CR>

let g:LanguageClient_serverCommands = {
    \ 'haskell': ['hie-wrapper', '--lsp'],
    \ }

" GHC mod
nmap <leader>tt :GhcModType<CR>
nmap <leader>tc :GhcModTypeClear<CR>
nmap <leader>ti :GhcModTypeInsert<CR>
nmap <leader>cc :GhcModSplitFunCase<CR>
