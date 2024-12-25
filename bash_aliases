colorflag="--color=auto"

alias ls='ls -G ${colorflag}' # Compact view, show colors
alias la='ls -AF ${colorflag}' # Compact view, show hidden
alias ll='ls -al ${colorflag}'
alias l='ls -a ${colorflag}'
alias l1='ls -1 ${colorflag}'

alias ..='cd ..' # Go up one directory
alias ...='cd ../..' # Go up two directories
alias ....='cd ../../..' # Go up three directories

alias gdb='gdb -tui'
