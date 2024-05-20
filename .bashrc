# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# Dot file configuration inspired by https://news.ycombinator.com/item?id=11071754.
# This post has instructions on how to clone the config:
# https://www.atlassian.com/git/tutorials/dotfiles
# Initialize with `git init --bare $HOME/.dotfiles

alias config-git="git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME"

function git() {
  if [[ $@ == "log" ]]; then
    command git log --all --decorate --graph
  else
    command git "$@"
  fi
}

# List everything by default unless we're in $HOME
function ls() {
    if  [[ $PWD == $HOME ]]; then
      command ls $@
  else
      command ls -a $@
  fi
}

PATH="$PATH:$HOME/.local/bin"
PATH="$HOME/bin:$PATH"
export PATH

# Had the following error once when running `yarn next`. This
# export fixed it. https://stackoverflow.com/questions/69692842/
# error-message-error0308010cdigital-envelope-routinesunsupported
#
# This leaves open a security hole. Only used it locally!
# export NODE_OPTIONS=--openssl-legacy-provider

# Can't use nano as editor because it doesn't work in a shell running inside
# Emacs (Emacs and nano key bindings conflict). Can't use vim either because
# vim doesn't come with with Fedora by default. So vi it is!
export EDITOR="vi"
