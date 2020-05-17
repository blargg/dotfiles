
# load shell settings
. $HOME/.commonsh/import.sh

# fix TERMINFO for nix shell
if [ -n "$IN_NIX_SHELL" ]; then
    export TERMINFO=/run/current-system/sw/share/terminfo

    # reload terminfo
    real_TERM=$TERM; TERM=xterm; TERM=$real_TERM; unset real_TERM
fi

[ -x "$(command -v zoxide)" ] && eval "$(zoxide init bash)"
