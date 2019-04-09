if [[ -n "$PS1" ]] ; then

# {{{ Paths

SYSTEM_PYTHON_BIN="/Users/$USER/Library/Python/2.7/bin/"
[[ -d "$SYSTEM_PYTHON_BIN" ]] && export PATH=$PATH:$SYSTEM_PYTHON_BIN

export PYTHONSTARTUP=$HOME/.pythonrc.py

# }}}

# {{{ Git

export GIT_COMMITTER_NAME="Berker Peksag"
export GIT_COMMITTER_EMAIL="berker.peksag@gmail.com"

# }}}

# {{{ Helpers

kport() {
    sudo kill `sudo lsof -t -i:"$1"`
}

pathremove() {
    local IFS=':'
    local NEWPATH
    local DIR
    local PATHVARIABLE=${2:-PATH}
    for DIR in ${!PATHVARIABLE} ; do
        if [ "$DIR" != "$1" ] ; then
            NEWPATH=${NEWPATH:+$NEWPATH:}$DIR
        fi
    done
    export $PATHVARIABLE="$NEWPATH"
}

unckecked_pathappend() {
    pathremove "$1" "$2"
    local PATHVARIABLE=${2:-PATH}
    export $PATHVARIABLE="${!PATHVARIABLE:+${!PATHVARIABLE}:}$1"
}

pathappend() {
    test ! -d "${1}" && return 0
    unckecked_pathappend "$1" "$2"
}

# Create a new directory and enter it
mkd() {
    mkdir -p "$@" && cd "$@"
}

# Create a new directory, enter it, create a virtualenv and activate it
pymkd() {
    mkdir -p "$@" && cd "$@" && python3.4 -m venv "venv" && source ./venv/bin/activate
}

cvenv() {
    python3.4 -m venv "venv" && source ./venv/bin/activate
}

venv() {
    source venv/bin/activate
}

dvenv() {
    deactivate
}

# }}}

# {{{ Prompt

parse_git_branch() {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1]/"
}

export PS1='\u@\h \[\033[1;33m\]\w\[\033[0m\]$(parse_git_branch)$ '

# }}}

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
