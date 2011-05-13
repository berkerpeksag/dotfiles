(add-to-list 'load-path "~/.emacs.d/")

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
