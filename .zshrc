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

# TODO this should be removed. Manage startx differently
if [[ -z $DISPLAY ]] && [[ $(tty) = /dev/tty1 ]]; then
	exec startx
fi

# bind Ctrl-O to ranger-cd:
bindkey -s '^O' '^qranger-cd\n'
