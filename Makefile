DOTFILES = $(PWD)

work:: clean basic
basic:: core bash git config python vim tmux
home:: clean basic scripts weechat python-hg
python:: python-core python-config

update-submodules::
	@git submodule foreach git pull origin master

core::
	@sudo apt-get update
	@sudo apt-get install -y git-core mercurial sqlite3 build-essential curl libcurl3 git-gui terminator weechat vim
	@echo Core libraries are installed.

python-core::
	@sudo apt-get install -y python-sqlite python-setuptools python-pip python-dev
	@sudo apt-get build-dep python3
	@echo Python environment is installed.

python-config::
	@ln -fs $(DOTFILES)/pythonrc.py $(HOME)/.pythonrc.py
	@cp $(DOTFILES)/pypirc-dist $(HOME)/.pypirc
	@echo Python config files are symlinked.

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

tmux::
	@ln -fs $(DOTFILES)/tmux.conf	$(HOME)/.tmux.conf
	@echo tmux is symlinked.

weechat::
	@ln -fns $(DOTFILES)/weechat	$(HOME)/.weechat
	@cp $(HOME)/.weechat/irc.conf.dist $(HOME)/.weechat/irc.conf

config::
	@ln -fns $(DOTFILES)/config/terminator	$(HOME)/.config/terminator
	@ln -fs $(DOTFILES)/config/flake8	$(HOME)/.config/flake8
	@echo Misc config files are symlinked.

clean::
	@rm -rf ~/.weechat ~/.gitconfig ~/.bashrc ~/.bash_aliases
	@rm -rf ~/.fonts ~/.vim ~/.vimrc
	@rm -rf ~/.pythonrc.py ~/.config/terminator ~/.hgrc ~/.tmux.conf
	@rm -rf ~/scripts ~/.bash_profile
	@rm -f ~/.config/flake8

.PHONY: clean config scripts
