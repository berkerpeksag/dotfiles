DOTFILES = $(PWD)
SCRIPTS = $(DOTFILES)/scripts

work:: basic
basic:: emacs tmux bash git config python
home:: basic scripts mozilla
python:: core python-core
mozilla:: core mozilla-core mozilla-config

core::
	@sudo apt-get update && apt-get upgrade && apt-get install git-core sqlite3 \
	build-essential nginx emacs23 curl libcurl3
	@echo Core libraries are installed.

python-core::
	@sudo apt-get install python-sqlite python-setuptools python-pip python-dev
	@echo Python environment is installed.

mozilla-core::
	@sudo apt-get install mercurial
	@echo Mozilla dependencies are installed.

mozilla-config::
	@ln -fs $(DOTFILES)/.mozconfig ${HOME}/.mozconfig
	@echo .mozconfig is symlinked.

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

config::
	@ln -fns $(DOTFILES)/.config	$(HOME)/.config
	@ln -fns $(DOTFILES)/.weechat	$(HOME)/.weechat
	@echo Misc config files are symlinked.

# Not finished yet.
#scripts::
#	@cp $(SCRIPTS)/x-art.sample.sh $(SCRIPTS)/met-art

# TODO(berker): Add `clean` command.
