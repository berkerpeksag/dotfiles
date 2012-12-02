DOTFILES = $(PWD)
SCRIPTS = $(DOTFILES)/scripts

work:: basic
basic:: emacs tmux bash git config python vim
home:: basic scripts mozilla
python:: core python-core python-tools python-config
mozilla:: core mozilla-core mozilla-config

core::
	@sudo apt-get update
	@sudo apt-get upgrade
	@sudo apt-get install git-core sqlite3 build-essential nginx emacs23 curl libcurl3 git-gui
	@echo Core libraries are installed.

python-core::
	@sudo apt-get install python-sqlite python-setuptools python-pip python-dev
	@echo Python environment is installed.

python-tools::
	@sudo pip install virtualenv
	@sudo apt-get install terminator
	@ln -fns $(DOTFILES)/.config/terminator	$(HOME)/.config/terminator
	@echo Python tools are installed.

python-config::
	@ln -fs $(DOTFILES)/.pythonrc.py ${HOME}/.pythonrc.py
	@ln -fns $(DOTFILES)/.autoenv	${HOME}/.autoenv
	@echo Python confif files are symlinked.

mozilla-core::
	@sudo apt-get install mercurial
	@echo Mozilla dependencies are installed.

mozilla-config::
	@ln -fs $(DOTFILES)/.mozconfig ${HOME}/.mozconfig
	@echo .mozconfig is symlinked.

emacs::
	@ln -fs $(DOTFILES)/.emacs	${HOME}/.emacs
	@ln -fns $(DOTFILES)/.emacs.d	${HOME}/.emacs.d
	@git gsu
	@echo Submodules are activated.
	@echo Emacs is symlinked.

vim:: vim-config vim-fonts

vim-config::
	@ln -fs ${DOTFILES}/.vimrc	${HOME}/.vimrc
	@ln -fns ${DOTFILES}/.vim	${HOME}/.vim
	@git gsu
	@echo Submodules are activated.
	@echo Vim is symlinked.

vim-fonts::
	@ln -fns ${DOTFILES}/.fonts	${HOME}/.fonts
	@echo .fonts are symlinked.

bash::
	@ln -fs $(DOTFILES)/.bashrc ${HOME}/.bashrc
	@ln -fs $(DOTFILES)/.bash_aliases ${HOME}/.bash_aliases
	@echo .bashrc and .bash_aliases are symlinked.

git::
	@ln -fs $(DOTFILES)/.gitconfig ${HOME}/.gitconfig
	@ln -fs $(DOTFILES)/.gitignore ${HOME}/.gitignore
	@echo .gitconfig and .gitignre are symlinked.

tmux::
	@ln -fs $(DOTFILES)/.tmux.conf	${HOME}/.tmux.conf
	@echo tmux is symlinked.

config::
	@ln -fns $(DOTFILES)/.weechat	$(HOME)/.weechat
	@ln -fns ${DOTFILES}/.config	${HOME}/.config
	@echo Misc config files are symlinked.

clean::
	@find . -name "*.elc" -exec rm {} \;

# Test command
foo::
	@git gsu
	@echo Submodules are activated.

# TODO(berker): Add `clean` command.
