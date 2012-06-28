(add-to-list 'load-path "~/.emacs.d/")

(load "python-config")
(load "settings")
(load "mozilla")

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

(require 'php-mode)

(require 'gist)

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-clarity)

(require 'smooth-scrolling)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-empty-lines t)
 '(safe-local-variable-values (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
