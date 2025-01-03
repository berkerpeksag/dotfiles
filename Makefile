DOTFILES = $(PWD)

basic:: git config python vim emacs
home:: clean core basic bash
work:: clean basic zsh
python:: python-config

core::
	@sudo apt update
	@sudo apt install -y build-essential bash-completion git git-gui vim emacs
	@echo Core libraries are installed.

python-config::
	@ln -fs $(DOTFILES)/pythonrc.py $(HOME)/.pythonrc.py
	@echo Python config files are symlinked.

vim:: vim-config

vim-config::
	@ln -fs $(DOTFILES)/vimrc	$(HOME)/.vimrc
	@ln -fns $(DOTFILES)/vim	$(HOME)/.vim
	@git submodule update --init --recursive
	@echo Submodules are activated.
	@echo Vim is symlinked.

emacs::
	@ln -fs $(DOTFILES)/emacs	$(HOME)/.emacs
	@echo Emacs is symlinked.

bash::
	@ln -fs $(DOTFILES)/bashrc $(HOME)/.bashrc
	@ln -fs $(DOTFILES)/bash_aliases $(HOME)/.bash_aliases
	@ln -fs $(DOTFILES)/bash_profile $(HOME)/.bash_profile
	@echo .bashrc, .bash_aliases and .bash_profile are symlinked.

zsh::
	@ln -fs $(DOTFILES)/zshrc $(HOME)/.zshrc
	@ln -fs $(DOTFILES)/zsh_aliases $(HOME)/.zsh_aliases
	@ln -fs $(DOTFILES)/zprofile $(HOME)/.zprofile
	@echo .zshrc, .zsh_aliases and .zprofile are symlinked.

git::
	@ln -fs $(DOTFILES)/gitconfig $(HOME)/.gitconfig
	@echo .gitconfig and .gitignore are symlinked.

clean::
	@rm -rf ~/.bashrc ~/.bash_aliases ~/.bash_profile
	@rm -rf ~/.zshrc ~/.zsh_aliases ~/.zprofile
	@rm -rf ~/.gitconfig ~/.vim ~/.vimrc ~/.emacs

.PHONY: clean config
