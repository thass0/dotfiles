. "$HOME/.cargo/env"
. "$HOME/.ghcup/env"

PATH="$PATH:$HOME/.local/bin"
PATH="$PATH:$HOME/.config/emacs/bin"
PATH="$PATH:$HOME/.emacs.d/bin"
PATH="$PATH:$HOME/.nimble/bin"
PATH="$HOME/bin:$PATH"
export PATH

# Had the following error once when running `yarn next`. This
# export fixed it. https://stackoverflow.com/questions/69692842/
# error-message-error0308010cdigital-envelope-routinesunsupported
#
# This leaves open a security hole. Only used it locally!
export NODE_OPTIONS=--openssl-legacy-provider

# URL for motes-share.
export MOTES_URL="https://thassilo.work"

export EDITOR="nano"
