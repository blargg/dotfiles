setlocal tags=./rusty-tags.vi;/
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!
let g:fzf_tags_command = 'rusty-tags vi --quiet --start-dir=.'

vmap gq :RustFmtRange<CR>

nmap <leader>m :wa <bar> make check <bar> cwindow<CR><CR>
nmap <leader>t :wa <bar> make test <bar> cwindow<CR><CR>
