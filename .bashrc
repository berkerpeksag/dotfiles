if [[ -n "$PS1" ]] ; then

# {{{ Paths

GAE_HOME=home/berker/google/appengine/python
export JAVA_HOME=/usr/lib/jvm/java-6-sun
PATH=$PATH:$GAE_HOME:$JAVA_HOME/bin
export PATH

# }}}

# {{{ Settings

export EDITOR="emacs -nw"

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
