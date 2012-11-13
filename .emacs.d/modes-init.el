(require 'auto-complete)
(global-auto-complete-mode t)

(autoload 'markdown-mode "markdown-mode.el"
 "Major mode for editing Markdown files" t)
(setq auto-mode-alist
 (cons '("\\.md" . markdown-mode) auto-mode-alist))

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(require 'linum)
(require 'cl)
(require 'smooth-scrolling)
