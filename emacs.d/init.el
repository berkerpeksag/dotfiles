;;; Modes path
(setq emacs-submodules-path "~/.emacs.d/modes/")

(dolist (submodule (directory-files emacs-submodules-path t "\\w+"))
    (when (file-directory-p submodule)
          (add-to-list 'load-path submodule)))

;;; Theme

(require 'color-theme)
(setq color-theme-is-global t)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-clarity)))

;;; Modes

(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsm$" . js2-mode))
(setq js2-language-version 180)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(require 'fullscreen)
(fullscreen-toggle)

(require 'auto-complete)
(global-auto-complete-mode t)

(autoload 'markdown-mode "markdown-mode.el"
 "Major mode for editing Markdown files" t)
(setq auto-mode-alist
 (cons '("\\.md" . markdown-mode) auto-mode-alist))

(autoload 'python-mode "python-mode"
 "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(require 'linum)
(global-linum-mode 1) ; display line numbers in margin.

(require 'smooth-scrolling)

(require 'golden-ratio)
(golden-ratio-enable)
; If you want to disable automatic resizing done by golden-ratio,
; just invoke (golden-ratio-disable)

;;; Settings

(global-hl-line-mode 1) ; turn on highlighting current line

; Visual Line mode provides support for editing by visual lines.
; It turns on word-wrapping in the current buffer, and rebinds C-a, C-e,
; and C-k to commands that operate by visual lines instead of logical
; lines.
(global-visual-line-mode 1) ; 1 for on, 0 for off.

;; save session state when you quit emacs
(setq
 desktop-dirname "~"
 desktop-base-file-name "emacs.desktop"
 desktop-base-lock-name "lock"
 desktop-path (list desktop-dirname)
 desktop-save t
 desktop-load-locked-desktop nil)
(desktop-save-mode 1)

;;; Keybindings

(global-set-key (kbd "<C-prior>") 'previous-user-buffer) ; Ctrl+PageUp
(global-set-key (kbd "<C-next>") 'next-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "<C-S-prior>") 'previous-emacs-buffer) ; Ctrl+Shift+PageUp
(global-set-key (kbd "<C-S-next>") 'next-emacs-buffer) ; Ctrl+Shift+PageDown

(global-set-key (kbd "M-3") 'delete-other-windows) ; expand current pane
(global-set-key (kbd "M-4") 'split-window-vertically) ; split pane top/bottom
(global-set-key (kbd "M-s") 'other-window) ; cursor to other pane
(global-set-key (kbd "M-0") 'delete-window) ; close current pane

(global-set-key (kbd "<f5>") 'find-file) ; Open file or dir
(global-set-key (kbd "<f6>") 'kill-this-buffer) ; Close file

;; If you want to be able to M-x without meta (phones, etc)
(global-set-key (kbd "C-c x") 'execute-extended-command)

;; Bind C-c s to eshell globally
(global-set-key (kbd "C-c s") 'eshell)

;;; Settings

(set-default-font "Ubuntu Mono-14")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; See: http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste#toc5
(setq
 ; inter-program-paste-function 'x-cut-buffer-or-selection-value
 make-backup-files nil ; stop creating those backup~ files
 auto-save-default nil) ; stop creating those #autosave# files

(setq x-select-enable-clipboard t) ; X11 Copy & Paste to/from Emacs

;; Taken from http://stackoverflow.com/a/2706660/57823
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
 "Prevent annoying \"Active processes exist\" query when you quit Emacs."
 (flet ((process-list ())) ad-do-it))

;;; Customization

(custom-set-variables
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-empty-lines t)
 '(safe-local-variable-values
   (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil)
 '(indent-tabs-mode nil)
 '(tabs-width 4)
 '(require-final-newline t) ; Add newline at end of files
 '(uniquify-buffer-name-style (quote forward) nil (uniquify)))

(custom-set-faces
 '(diff-added ((t (:foreground "Green"))) 'now)
 '(diff-removed ((t (:foreground "Red"))) 'now))

;;; Functions

; http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
