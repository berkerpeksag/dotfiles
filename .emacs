(add-to-list 'load-path "~/.emacs.d/")

(load "python-config")
(load "settings")
(load "mozilla")

; Taken from http://stackoverflow.com/a/94277/57823
(defun set-frame-size-according-to-resolution ()
 (interactive)
 (if window-system
  (progn
   ;; use 120 char wide window for largeish displays
   ;; and smaller 80 column windows for smaller displays
   ;; pick whatever numbers make sense for you
   (if (> (x-display-pixel-width) 1280)
          (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
   ;; for the height, subtract a couple hundred pixels
   ;; from the screen height (for panels, menubars and
   ;; whatnot), then divide by the height of a char to
   ;; get the height we want
   (add-to-list 'default-frame-alist
    (cons 'height (/ (- (x-display-pixel-height) 200)
     (frame-char-height)))))))
(set-frame-size-according-to-resolution)

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

; Save the old theme for now.
;(require 'color-theme)
;(setq color-theme-is-global t)
;(color-theme-clarity)
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(require 'color-theme-solarized)

(require 'smooth-scrolling)

(add-to-list 'load-path "~/.emacs.d/rust-mode/")
(require 'rust-mode)

(custom-set-variables
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-empty-lines t)
 '(safe-local-variable-values (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil)
 '(indent-tabs-mode nil)
 '(require-final-newline t)) ;; Add newline at end of files

;; Taken from http://stackoverflow.com/a/2706660/57823
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
 "Prevent annoying \"Active processes exist\" query when you quit Emacs."
 (flet ((process-list ())) ad-do-it))

;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now))
