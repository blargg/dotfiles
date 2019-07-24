
# load shell settings
if [ -d "${HOME}/.commonsh" ] ; then
	for file in $HOME/.commonsh/* ; do
		. "$file"
	done
fi

# fix TERMINFO for nix shell
if [ -n "$IN_NIX_SHELL" ]; then
    export TERMINFO=/run/current-system/sw/share/terminfo

    # reload terminfo
    real_TERM=$TERM; TERM=xterm; TERM=$real_TERM; unset real_TERM
fi
