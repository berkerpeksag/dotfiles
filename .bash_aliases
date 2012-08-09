alias _=sudo

# Directory
alias md='mkdir -p'

# List directory contents
alias ls='ls -G' # Compact view, show colors
alias la='ls -AF' # Compact view, show hidden
alias ll='ls -al'
alias l='ls -a'
alias l1='ls -1'

alias ..='cd ..' # Go up one directory
alias ...='cd ../..' # Go up two directories
alias ....='cd ../../..' # Go up two directories
alias ..-="cd -" # Go back

# Mercurial
alias hs='hg status'
alias hsum='hg summary'
alias hcm='hg commit -m'

# Mozilla
alias fx='dist/bin/firefox -P clean -no-remote'
