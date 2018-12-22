" vimrc
" Original author: Saleem Abdulrasool <compnerd@compnerd.org>
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
Plugin 'chrisbra/Recover.vim'
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
Plugin 'eagletmt/ghcmod-vim'
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
set wildmode=longest,full  " Match the longest substring, complete with first
set wildignore=*.o,*~      " Ignore temp files in wildmenu
set scrolloff=3            " Show 3 lines of context during scrolls
set sidescrolloff=2        " Show 2 columns of context during scrolls
set backspace=2            " Normal backspace behavior
set textwidth=80           " Break lines at 80 characters
set hidden                 " Allow flipping of buffers without saving
set noerrorbells           " Disable error bells
set visualbell             " Turn visual bell on
set t_vb=                  " Make the visual bell emit nothing
set showcmd                " Show the current command
set secure                 " Be safe when using modeline and files
set exrc                   " Load the .vimrc in the current folder too

let mapleader=" "    " Set leader to splace

if v:version > 703
   set formatoptions+=j
endif

" --- Command-t options ---
let g:CommandTMaxHeight=10

set diffopt+=iwhite

" ---- Filetypes ----
if has('syntax')
   syntax on
endif

if has('eval')
   filetype on             " Detect filetype by extension
   filetype indent on      " Enable indents based on extensions
   filetype plugin on      " Load filetype plugins
endif

"TODO consider moving this to ftplugin files
if has('autocmd')
   autocmd FileType html,htmldjango setlocal foldmethod=indent
endif


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

" Enable modelines only on secure vim
if (v:version == 603 && has("patch045")) || (v:version > 603)
   set modeline
   set modelines=3
else
   set nomodeline
endif

" Set colorscheme
if has("gui_running")
   colorscheme wombat256mod
elseif &t_Co == 256
   colorscheme wombat256mod
elseif &t_Co == 88
   colorscheme wombat
else
   colorscheme wombat
endif

" Show trailing whitespace visually
" Shamelessly stolen from Ciaran McCreesh <ciaranm@gentoo.org>
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

   " subversion commit messages need not be backed up
   autocmd BufRead svn-commit.tmp :setlocal nobackup

   " mutt does not like UTF-8
   autocmd BufRead,BufNewFile *
      \ if &ft == 'mail' | set fileencoding=iso8859-1 | endif

   " fix up procmail rule detection
   autocmd BufRead procmailrc :setfiletype procmail
endif

" ---- cscope/ctags setup ----
if has('cscope') && executable('cscope') == 1
   " Search cscope and ctags, in that order
   set cscopetag
   set cscopetagorder=0

   set nocsverb
   if filereadable('cscope.out')
      cs add cscope.out
   endif
   set csverb
endif

" ---- Key Mappings ----
"
" Better window split navigation
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h

nmap <c-t> :NERDTreeToggle<CR>

" Ropevim python code navigation

" Clear search colors
nmap <silent> <leader>n :silent :nohlsearch<CR>

" improved lookup
if has('eval')
   fun! GoDefinition()
      let pos = getpos(".")
      normal! gd
      if getpos(".") == pos
         exe "tag " . expand("<cword>")
      endif
   endfun

   nmap <C-]> :call GoDefinition()<CR>
endif

if has('autocmd')
   " Shortcuts
   if has('eval')
      fun! <SID>cabbrev()
         iab #i #include
         iab #I #include

         iab #d #define
         iab #D #define

         iab #e #endif
         iab #E #endif
      endfun

      autocmd FileType c,cpp :call <SID>cabbrev()

      autocmd BufNewFile,BufRead wscript set filetype=python syntax=python
      autocmd BufNewFile,BufRead *.mm set filetype=noweb
      autocmd BufNewFile,BufRead *.scala set filetype=scala
      autocmd BufNewFile,BufRead *.proto set filetype=proto
      autocmd BufNewFile,BufRead *.atomo set filetype=atomo
      autocmd BufNewFile,BufRead *.atomo setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2 commentstring=--\ %s

      autocmd FileType python set omnifunc=pythoncomplete#Complete
      let g:SuperTabDefaultCompletionType = "context"
   endif

   " make tab reindent in normal mode
   autocmd FileType c,cpp,cs,java nmap <Tab> =0<CR>
endif

" Append modeline after last line in buffer.
" Use substitute() (not printf()) to handle '%%s' modeline in LaTeX files.
if has('eval')
   fun! AppendModeline()
      let save_cursor = getpos('.')
      let append = ' vim: set ts='.&tabstop.' sw='.&shiftwidth.' tw='.&textwidth.': '
      $put =substitute(&commentstring, '%s', append, '')
      call setpos('.', save_cursor)
   endfun
   nnoremap <silent> <Leader>ml :call AppendModeline()<CR>
endif

" shifted arrows are stupid
inoremap <S-Up> <C-O>gk
noremap  <S-Up> gk
inoremap <S-Down> <C-O>gj
noremap  <S-Down> gj

" Y should yank to EOL
map Y y$

" vK is stupid
vmap K k

" :W and :Q are annoying
if has('user_commands')
   command! -nargs=0 -bang Q q<bang>
   command! -nargs=0 -bang W w<bang>
   command! -nargs=0 -bang WQ wq<bang>
   command! -nargs=0 -bang Wq wq<bang>
endif

" just continue
nmap K K<cr>

" stolen from auctex.vim
if has('eval')
   fun! EmacsKill()
      if col(".") == strlen(getline(line(".")))+1
         let @" = "\<CR>"
         return "\<Del>"
      else
         return "\<C-O>D"
      endif
   endfun
endif

" w!! for sudo w!
"cmap w!! w !sudo tee % >/dev/null

" Disable q and Q
map q <Nop>
map Q <Nop>

" Toggle numbers with F12
nmap <silent> <F12> :silent set number!<CR>
imap <silent> <F12> <C-O>:silent set number!<CR>

" Don't force column 0 for #
inoremap # X<BS>#

if (&term =~ "interix")
   map  <C-?> <DEL>
   map! <C-?> <DEL>
   map <C-[>[H <Home>
   map <C-[>[U <End>
elseif (&term =~ "^sun")
   map  <C-?> <DEL>
   map! <C-?> <DEL>
elseif (&term !~ "cons")
   map  <C-?> <BS>
   map! <C-?> <BS>
endif

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

" Terminal.app does not support back color erase
if ($TERM_PROGRAM ==# "Apple_Terminal" && $TERM_PROGRAM_VERSION <= 273)
   set t_ut=
endif

" Python specific stuff
if has('eval')
   let python_highlight_all = 1
   let python_slow_sync = 1
endif

" ---- OmniCpp ----
if v:version >= 700
   if has('autocmd')
      autocmd InsertLeave * if pumvisible() == 0|pclose|endif
   endif

   set completeopt=menu,menuone,longest

   let OmniCpp_MayCompleteDot = 1 " autocomplete with .
   let OmniCpp_MayCompleteArrow = 1 " autocomplete with ->
   let OmniCpp_MayCompleteScope = 1 " autocomplete with ::
   let OmniCpp_SelectFirstItem = 2 " select first item (but don't insert)
   let OmniCpp_NamespaceSearch = 2 " search namespaces in this and included files
   let OmniCpp_ShowPrototypeInAbbr = 1 " show function prototype (i.e. parameters) in popup window
   map <C-F12> :!$HOME/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR><CR>
   " TODO find a better fix for the above
   map <F6> :!$HOME/bin/ctags -R --c++-kinds=+pl --fields=+iaS --extra=+q .<CR><CR>
   " add current directory's generated tags file to available tags
   set tags+=./tags
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

" control-p
let g:ctrlp_prompt_mappings = {
   \ 'PrtClearCache()':      ['<c-w>'],
   \ 'PrtCurLeft()':         ['<c-h>', '<left>', '<c-^>'],
   \ 'PrtCurRight()':        ['<c-l>', '<right>'],
   \ }

" fzf settings
nmap <c-p> :Files<CR>
nmap <leader>p :Files<CR>
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

" GHC mod
nmap <leader>tt :GhcModType<CR>
nmap <leader>tc :GhcModTypeClear<CR>
nmap <leader>ti :GhcModTypeInsert<CR>
nmap <leader>cc :GhcModSplitFunCase<CR>
