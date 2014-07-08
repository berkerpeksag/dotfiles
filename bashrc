if [[ -n "$PS1" ]] ; then

# {{{ Paths

export SCRIPTS_HOME=/home/berker/dotfiles/scripts/
PATH=$PATH:$SCRIPTS_HOME
export PATH

export PYTHONSTARTUP=$HOME/.pythonrc.py

# }}}

# {{{ Settings

export PIP_DOWNLOAD_CACHE=~/.pip-cache

# }}}

# {{{ Git

export GIT_COMMITTER_NAME="Berker Peksag"
export GIT_COMMITTER_EMAIL="berker.peksag@gmail.com"

# }}}

# {{{ Helpers

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

# Create a new "Python" directory, enter it, create
# a virtualenv and activate it
pymkd() {
    mkdir -p "$@" && cd "$@" && virtualenv "venv" && source ./venv/bin/activate
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

parse_hg_branch() {
    hg branch 2> /dev/null | awk '{printf "[%s]", $1}'
}

export PS1='\u@\h \[\033[1;33m\]\w\[\033[0m\]$(parse_git_branch)$(parse_hg_branch)$ '

# }}}

# {{{ Mozilla

mk() {
    last=${!#};
    for folder in "$@"; do
        make -sC $folder -j4
        RESULT=$?
        if [ $RESULT -gt 0 ]; then
            echo Error making $folder
            return $RESULT
        else
            echo Made $folder
        fi
    done
}

# }}}

export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
export HISTCONTROL=ignoreboth

shopt -s histappend
shopt -s checkwinsize

eval "`dircolors -b`"

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

fi

source ~/.autoenv/activate.sh
