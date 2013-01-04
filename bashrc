if [[ -n "$PS1" ]] ; then

# {{{ Paths

GAE_HOME=/home/berker/google/appengine
export JAVA_HOME=/usr/lib/jvm/java-6-sun
export SCRIPTS_HOME=/home/berker/dotfiles/scripts/
PATH=$PATH:$GAE_HOME:$JAVA_HOME/bin:$SCRIPTS_HOME
export PATH

export PYTHONSTARTUP=$HOME/.pythonrc.py

# }}}

# {{{ Settings

export EDITOR="emacs -nw"
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
function mkd() {
    mkdir -p "$@" && cd "$@"
}

# }}}

# {{{ Prompt

function parse_git_dirty {
    [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
    git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/[\1$(parse_git_dirty)]/"
}

export PS1='\u@\h \[\033[1;33m\]\w\[\033[0m\]$(parse_git_branch)$ '

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

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
source ~/.autoenv/activate.sh