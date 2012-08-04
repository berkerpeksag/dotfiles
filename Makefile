DOTFILES = $(PWD)
SCRIPTS = $(DOTFILES)/scripts

all:: emacs tmux scripts

emacs::
	@ln -fs $(DOTFILES)/.emacs	${HOME}/.emacs
	@ln -fns $(DOTFILES)/.emacs.d	${HOME}/.emacs.d
	@echo Emacs is symlinked.

tmux::
	@ln -fs $(DOTFILES)/.tmux.conf	${HOME}/.tmux.conf
	@echo tmux is symlinked.

# Not finished yet.
scripts::
	@cp $(SCRIPTS)/x-art.sample.sh $(SCRIPTS)/met-art

# TODO(berker): Add `clean` command.
