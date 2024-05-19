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
