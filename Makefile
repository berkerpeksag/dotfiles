DOTFILES = $(PWD)
SCRIPTS = $(DOTFILES)/scripts

all:: emacs tmux bash git scripts

emacs::
	@ln -fs $(DOTFILES)/.emacs	${HOME}/.emacs
	@ln -fns $(DOTFILES)/.emacs.d	${HOME}/.emacs.d
	@echo Emacs is symlinked.

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

# Not finished yet.
#scripts::
#	@cp $(SCRIPTS)/x-art.sample.sh $(SCRIPTS)/met-art

# TODO(berker): Add `clean` command.
