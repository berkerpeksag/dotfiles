case $- in *i*) . ~/.bashrc;; esac

test -r "$HOME/.opam/opam-init/init.sh" && . "$HOME/.opam/opam-init/init.sh" > /dev/null 2>&1 || true

export PATH="/usr/local/go/bin:$PATH"

export NVM_DIR="$HOME/.nvm"
test -r "$NVM_DIR/nvm.sh" && . "$NVM_DIR/nvm.sh" > /dev/null 2>&1 || true
test -r "$NVM_DIR/bash_completion" && . "$NVM_DIR/bash_completion" > /dev/null 2>&1 || true
