#! /bin/sh
# aliases for all shells

# common
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias cdh='urxvt -cd $(pwd) &'
alias vim='nvim'
alias pj='pijul'
alias now='date "+(%a)%D  %I:%M:%S %p"'
alias nixpkgs='nix repl "<nixpkgs>"'
alias mx='tmux'

# admin
alias sys='systemctl'
alias ssys='sudo systemctl'

# dev
alias cwatch="cargo watch --clear"
alias nix-ipython='nix-shell --run ipython'
alias t="task"
alias i="task +inbox"
alias ts="task +soon"
alias tw="timew"
