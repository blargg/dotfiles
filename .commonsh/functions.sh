# /bin/sh

# runs ranger and changes directory of the shell to match
function ranger-cd {
	tempfile='/tmp/.ranger.ranger-cd'
	ranger --choosedir="$tempfile" "${@:-$(pwd)}"
	test -f "$tempfile" &&
	if [  "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
		cd -- "$(cat "$tempfile")"
	fi
	rm -f -- "$tempfile"
}

# password generator
function genpass() {
	if [ ! "$1" ]; then
		echo "Usage: $0 20"
		echo "for a random, 20 char password."
		return 1
	fi
	dd if=/dev/urandom count=1 2>/dev/null | tr -cd 'A-Za-z0-9!@#$%^&*()_+' |
		cut -c-$1
}

function task.advancetime() {
    task +soon2 mod -soon2 +soon
}

function td() {
    task $@ mod -soon -soon2 +today
}
