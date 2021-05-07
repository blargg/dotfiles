#! /bin/sh
# variables for use
export BROWSER="google-chrome-stable"
export EDITOR="nvim"
export PAGER="less"
export PATH="$PATH:$HOME/.local/bin"
# Cargo install location
export PATH="$PATH:$HOME/.cargo/bin"
export _ZO_EXCLUDE_DIRS="$HOME/private"

# In memory only history
unset HISTFILE

eval $(dircolors -b ~/.dircolors)
