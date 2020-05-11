autoload -Uz compinit promptinit vcs_info
compinit
promptinit
prompt walters
autoload -U zcalc

### watch ###
watch=( all )
export LOGCHECK=30
export WATCHFMT=$'\e[00;00m\e[01;36m'" -- %n@%m has %(a.logged in.logged out) --"$'\e[00;00m'

# load shell settings
if [ -d "${HOME}/.commonsh" ] ; then
	for file in $HOME/.commonsh/* ; do
		source "$file"
	done
fi

# load zsh specific files
if [ -d "${HOME}/.zsh" ] ; then
	for file in $HOME/.zsh/* ; do
		source "$file"
	done
fi

# bind Ctrl-O to ranger-cd:
bindkey -s '^O' '^qranger-cd\n'
bindkey -v

# zle: update VIMMODE on mode chagne
function zle-line-init zle-keymap-select {
    vimode="${${KEYMAP/vicmd/[n]}/(main|viins)/[i]}"
    PROMPT="${vimode} %B%(?..[%?] )%b%n%F{240}@%f%U%m%u>"
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select
