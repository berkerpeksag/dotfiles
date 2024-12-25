DOTFILES = $(PWD)

work:: clean basic
basic:: core bash git config python vim
home:: clean basic
python:: python-core python-config

update-submodules::
	@git submodule foreach git pull origin master

core::
	@sudo apt-get update
	@sudo apt-get install -y git-core sqlite3 build-essential curl libcurl3 git-gui vim
	@echo Core libraries are installed.

python-config::
	@ln -fs $(DOTFILES)/pythonrc.py $(HOME)/.pythonrc.py
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

clean::
	@rm -rf ~/.gitconfig ~/.bashrc ~/.bash_aliases
	@rm -rf ~/.fonts ~/.vim ~/.vimrc ~/.bash_profile

.PHONY: clean config
