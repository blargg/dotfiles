autoload -Uz compinit promptinit vcs_info
compinit
promptinit
prompt walters
autoload -U zcalc

### watch ###
watch=( all )
export LOGCHECK=30
export WATCHFMT=$'\e[00;00m\e[01;36m'" -- %n@%m has %(a.logged in.logged out) --"$'\e[00;00m'

source $HOME/.commonsh/import.sh
source $HOME/.zsh/import

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec dbus-run-session sway
fi
