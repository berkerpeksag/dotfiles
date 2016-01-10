# Detect which `ls` flavor is in use
# Taken from mathiasbynens/dotfiles
if ls --color > /dev/null 2>&1; then # GNU `ls`
    colorflag="--color=auto"
else # OS X `ls`
    colorflag="-G"
fi

# List directory contents
alias ls='ls -G ${colorflag}' # Compact view, show colors
alias la='ls -AF ${colorflag}' # Compact view, show hidden
alias ll='ls -al ${colorflag}'
alias l='ls -a ${colorflag}'
alias l1='ls -1 ${colorflag}'

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

# OS X specific aliases

alias afk="/System/Library/CoreServices/Menu\ Extras/User.menu/Contents/Resources/CGSession -suspend"

# Recursively delete `.DS_Store` files
alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"
