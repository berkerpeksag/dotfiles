# List directory contents
alias ls='ls -G --color=auto' # Compact view, show colors
alias la='ls -AF --color=auto' # Compact view, show hidden
alias ll='ls -al --color=auto'
alias l='ls -a --color=auto'
alias l1='ls -1 --color=auto'

alias ..='cd ..' # Go up one directory
alias ...='cd ../..' # Go up two directories
alias ....='cd ../../..' # Go up three directories

# Unix tools
alias uname='python -m platform'

# Tmux
alias tmux='tmux -2'

# Tunnelling
# See for a great article about SSH tunneling:
# https://calomel.org/firefox_ssh_proxy.html
alias work='ssh -C2qTnN -D 8383 wakefield@berkerpeksag.com'

alias fap='fab'
alias fapfapfap='fab'
