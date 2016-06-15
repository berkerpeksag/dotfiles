case $- in *i*) . ~/.bashrc;; esac

if which brew &> /dev/null && [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi
