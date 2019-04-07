case $- in *i*) . ~/.bashrc;; esac

if hash brew 2> /dev/null && [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi
