setlocal tags=./rusty-tags.vi;/
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!
let g:fzf_tags_command = 'rusty-tags vi --quiet --start-dir=.'
