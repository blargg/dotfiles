nmap <silent> gf :call OpenOrPrompt(1)<CR>
set makeprg=nix-instantiate\ --parse\ \%
set errorformat=error:%m\ at\ %f:%l:%c
