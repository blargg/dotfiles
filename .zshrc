
source $HOME/.commonsh/import.sh
source $HOME/.zsh/import

if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec dbus-run-session sway
fi
