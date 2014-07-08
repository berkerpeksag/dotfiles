DOTFILES = $(PWD)
SCRIPTS = $(DOTFILES)/scripts

work:: basic
basic:: emacs tmux bash git config python vim
home:: basic scripts
python:: core python-core python-tools python-config

update-submodules::
	@git submodule foreach git pull origin master

core::
	@sudo apt-get update
	@sudo apt-get install -y git-core mercurial sqlite3 build-essential curl libcurl3 git-gui terminator weechat vim
	@echo Core libraries are installed.

python-core::
	@sudo apt-get install -y python-sqlite python-setuptools python-pip python-dev
	@echo Python environment is installed.

python-tools::
	@sudo pip install virtualenv
	@echo Python tools are installed.

python-config::
	@ln -fs $(DOTFILES)/pythonrc.py $(HOME)/.pythonrc.py
	@ln -fs $(DOTFILES)/pypirc-dist $(HOME)/.pypirc
	@ln -fns $(DOTFILES)/autoenv	$(HOME)/.autoenv
	@echo Python config files are symlinked.

emacs::
	@ln -fns $(DOTFILES)/emacs.d	$(HOME)/.emacs.d
	@git submodule update --init --recursive
	@echo Submodules are activated.
	@echo Emacs is symlinked.

vim:: vim-config vim-fonts

vim-config::
	@ln -fs $(DOTFILES)/vimrc	$(HOME)/.vimrc
	@ln -fns $(DOTFILES)/vim	$(HOME)/.vim
	@git submodule update --init --recursive
	@echo Submodules are activated.
	@echo Vim is symlinked.

vim-fonts::
	@ln -fns $(DOTFILES)/fonts	$(HOME)/.fonts
	@echo .fonts are symlinked.

bash::
	@ln -fs $(DOTFILES)/bashrc $(HOME)/.bashrc
	@ln -fs $(DOTFILES)/bash_aliases $(HOME)/.bash_aliases
	@ln -fs $(DOTFILES)/bash_profile $(HOME)/.bash_profile
	@echo .bashrc, .bash_aliases and .bash_profile are symlinked.

git::
	@ln -fs $(DOTFILES)/gitconfig $(HOME)/.gitconfig
	@echo .gitconfig and .gitignore are symlinked.

hg::
	@ln -fs $(DOTFILES)/hgrc $(HOME)/.hgrc
	@echo .hgrc is symlinked.

tmux::
	@ln -fs $(DOTFILES)/tmux.conf	$(HOME)/.tmux.conf
	@echo tmux is symlinked.

scripts::
	@ln -fns $(DOTFILES)/scripts	$(HOME)/scripts
	@echo scripts/ is symlinked.

config::
	@ln -fns $(DOTFILES)/weechat	$(HOME)/.weechat
	@cp $(HOME)/.weechat/irc.conf.dist $(HOME)/.weechat/irc.conf
	@ln -fns $(DOTFILES)/config	$(HOME)/.config
	@echo Misc config files are symlinked.
	@ln -fs $(DOTFILES)/gedrc	$(HOME)/.gedrc
	@ln -fns $(DOTFILES)/config/terminator	$(HOME)/.config/terminator

clean::
	@rm -rf ~/.weechat ~/.gitconfig ~/.bashrc ~/.bash_aliases
	@rm -rf ~/.fonts ~/.vim ~/.vimrc ~/.emacs.d ~/.autoenv
	@rm -rf ~/.pythonrc.py ~/.config/terminator ~/.hgrc ~/.tmux.conf
	@rm -rf ~/scripts .bash_profile

.PHONY: clean config scripts
