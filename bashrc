if [[ -n "$PS1" ]] ; then

export PYTHONSTARTUP=$HOME/.pythonrc.py

kport() {
    sudo kill `sudo lsof -t -i:"$1"`
}

mkd() {
    mkdir -p "$@" && cd "$@"
}

venv() {
    source venv/bin/activate
}

dvenv() {
    deactivate
}

open_in_chrome() {
    if [ $# -eq 0 ]; then
        echo "Usage: open_in_chrome <path-to-html-file>"
        return 1
    fi

    local wsl_path="$1"
    if [ ! -f "$wsl_path" ]; then
        echo "Error: File not found: $wsl_path"
        return 1
    fi

    local win_path
    win_path=$(wslpath -w "$wsl_path")
    if [ $? -ne 0 ]; then
        echo "Error: Failed to convert WSL path to Windows path."
        return 1
    fi

    # Change current directory to /mnt/c (valid Windows path) to avoid
    # UNC path warning.
    local current_dir
    current_dir=$(pwd)
    if ! cd /mnt/c; then
        echo "Error: Failed to change directory to /mnt/c. Ensure it exists."
        return 1
    fi

    if ! cmd.exe /C start chrome "$win_path"; then
        echo "Error: Failed to launch Chrome."
        cd "$current_dir" || echo "Warning: Failed to restore original directory."
        return 1
    fi

    if ! cd "$current_dir"; then
        echo "Warning: Failed to restore original directory."
        return 1
    fi
}

parse_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}

export PS1='\u@\h \[\033[1;33m\]\w\[\033[0m\]$(parse_git_branch)$ '

export HISTCONTROL=ignoreboth:erasedups
shopt -s histappend
shopt -s checkwinsize

if hash dircolors 2> /dev/null; then
    eval "`dircolors -b`"
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

fi
