;;; Paths

(let ((default-directory "~/.emacs.d/modes/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;; Packaging

(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; Theme

;; Alternative themes:
;; (load-theme 'molokai t)
;; (load-theme 'monokai t)

(load-theme 'soothe t)

;;; Modes

(require 'powerline)
(powerline-default-theme)

(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(autoload 'markdown-mode "markdown-mode.el"
 "Major mode for editing Markdown files" t)
(setq auto-mode-alist
 (cons '("\\.md" . markdown-mode) auto-mode-alist))

(autoload 'python-mode "python-mode"
 "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(global-linum-mode 1) ; display line numbers in margin.

(require 'ido)
(ido-mode t)

; If you want to disable automatic resizing done by golden-ratio,
; just invoke (golden-ratio-disable)
(require 'golden-ratio)
(golden-ratio-enable)

;;; Settings

;; highlight matching closing brackets
(show-paren-mode 1)

;; for Emacs Lisp programming
;;(eldoc-mode 1)

;; insert closing brackets automagically
(electric-pair-mode 1)

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

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;; Automatic white-space insertion after comma
(global-set-key (kbd ",")
                (lambda() (interactive) (insert ", ")))

;;; Settings

(set-default-font "Ubuntu Mono-14")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq mouse-wheel-scroll-amount '(1))

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; See: http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste#toc5
(setq
 ; inter-program-paste-function 'x-cut-buffer-or-selection-value
 x-select-enable-clipboard t ; X11 Copy & Paste to/from Emacs
 make-backup-files nil ; stop creating those backup~ files
 auto-save-default nil) ; stop creating those #autosave# files

;; Taken from http://stackoverflow.com/a/2706660/57823
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
 "Prevent annoying \"Active processes exist\" query when you quit Emacs."
 (flet ((process-list ())) ad-do-it))

;; Start with maximized window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
 '(menu-bar-mode nil)
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


;; A function to create new functions that look for a specific pattern
;; http://whattheemacsd.com/key-bindings.el-04.html
(require 'find-file-in-project)

(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Find file in project, with specific patterns
(global-unset-key (kbd "C-x C-o"))
(global-set-key (kbd "C-x C-o el")
                (ffip-create-pattern-file-finder "*.el"))
(global-set-key (kbd "C-x C-o js")
                (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-o py")
                (ffip-create-pattern-file-finder "*.py"))


;; Shutdown Emacs server instance
;; Usage: M-x shutdown-server
(defun shutdown-server ()
  "Save buffers and kill the server"
  (interactive)
  (save-buffers-kill-emacs))


(defun find-user-init-file ()
  "Edit the user-init-file, in another window"
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)


(defun comment-or-uncomment-line-or-region ()
  "Toggles commenting on the current line if no region is defined,
   otherwise toggles comments on the region."
  (interactive "*")
  (let ((use-empty-active-region t) (mark-even-if-inactive nil))
    (cond
     ((use-region-p) (comment-or-uncomment-region
                      (region-beginning) (region-end)))
     (t (comment-or-uncomment-region
         (line-beginning-position) (line-end-position))))))
