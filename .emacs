(add-to-list 'load-path "~/.emacs.d/")

(load "python-config")
(load "settings")
(load "mozilla")

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'whitespace)
(global-set-key (kbd "\C-c w") 'whitespace-mode)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'linum)

(require 'cl)

(require 'php-mode)

(require 'gist)

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-clarity)

(require 'smooth-scrolling)

