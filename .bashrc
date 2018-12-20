
# load shell settings
if [ -d "${HOME}/.commonsh" ] ; then
	for file in $HOME/.commonsh/* ; do
		. "$file"
	done
fi
