alias _=sudo

# Directory
alias md='mkdir -p'

# List directory contents
alias sl=ls
alias ls='ls -G' # Compact view, show colors
alias la='ls -AF' # Compact view, show hidden
alias ll='ls -al'
alias l='ls -a'
alias l1='ls -1'

alias ..='cd ..' # Go up one directory
alias ...='cd ../..' # Go up two directories
alias ....='cd ../../..' # Go up two directories
alias --="cd -" # Go back

# Mercurial
alias hs='hg status'
alias hsum='hg summary'
alias hcm='hg commit -m'

# Git
alias gs='git status'
alias gss='git status -s'
alias gl='git pull'
alias gb='git branch'
alias gsu='git submodule update --init --recursive'
