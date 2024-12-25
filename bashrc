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
