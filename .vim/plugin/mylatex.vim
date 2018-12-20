function! g:LaTexCompile()
    execute ":silent !pdflatex " . expand('%') . ">/dev/null &"
    redraw!
endfunction

function! g:LaTexShowPDF()
    let str=split(expand('%'), ".tex")
    execute ":silent !mimeopen " . str[0] . ".pdf>&/dev/null &"
    redraw!
endfunction

if has('autocmd')
   " assume *.tex is tex, not plaintex
   autocmd BufRead,BufNewFile *.tex set filetype=tex
endif
