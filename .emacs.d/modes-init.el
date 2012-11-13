(require 'auto-complete)
(global-auto-complete-mode t)

(require 'whitespace)
(global-set-key (kbd "\C-c w") 'whitespace-mode)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(autoload 'markdown-mode "markdown-mode.el"
 "Major mode for editing Markdown files" t)
(setq auto-mode-alist
 (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'linum)

(require 'cl)

(require 'gist)

(require 'smooth-scrolling)

(add-to-list 'load-path "~/.emacs.d/modes/rust-mode/")
(require 'rust-mode)
