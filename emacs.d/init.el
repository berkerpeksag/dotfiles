;;; init.el --- My Emacs initialization file

;; Paths

(let ((default-directory "~/.emacs.d/modes/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; Packaging

(require 'package)
(add-to-list 'package-archives
    '("gnu" .
      "http://elpa.gnu.org/packages/")
    '("melpa" .
      "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Theme

(load-theme 'molokai t)

;; Modes

(require 'hy-mode)
(setq hy-mode-inferior-lisp-command "hy --spy")

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

(require 'ido)
(ido-mode t)

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
  (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

(global-linum-mode 1) ; display line numbers in margin.
(setq linum-format "%6d ")

;; If you want to disable automatic resizing done by golden-ratio,
;; just invoke (golden-ratio-disable)
(require 'golden-ratio)
(golden-ratio-mode 1)


;; Settings

(set-default-font "Ubuntu Mono-14")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; make "yes or no" "y or n"
(fset 'yes-or-no-p 'y-or-n-p)

;; start with maximized window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq-default fill-column 80)

;; highlight matching closing brackets
(show-paren-mode 1)

;; insert closing brackets automagically
(electric-pair-mode 1)

(global-hl-line-mode 1) ; turn on highlighting current line

;; Visual Line mode provides support for editing by visual lines.
;; It turns on word-wrapping in the current buffer, and rebinds C-a, C-e,
;; and C-k to commands that operate by visual lines instead of logical
;; lines.
(global-visual-line-mode 1) ; 1 for on, 0 for off.

(column-number-mode t)
(cua-mode t)
(transient-mark-mode 1) ; No region when it is not highlighted
(icomplete-mode t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(blink-cursor-mode 0)

(set-fringe-mode 0)

(setq-default save-place t)
(setq-default indent-tabs-mode nil)

;; hide welcome screen
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(setq cua-auto-tabify-rectangles nil) ; Don't tabify after rectangle commands
(setq cua-keep-region-after-copy t) ; Standard Windows behaviour
(setq indicate-empty-lines t)
(setq tabs-width 4)
(setq require-final-newline t) ; Add newline at end of files
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator ":")
(setq mouse-wheel-scroll-amount '(1))

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


;; Keybindings

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


;; Custom defuns

;; taken from http://whattheemacsd.com/file-defuns.el-01.html
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


(defun find-user-init-file ()
  "Edit the user-init-file, in another window"
  (interactive)
  (find-file-other-window user-init-file))

(global-set-key (kbd "C-c I") 'find-user-init-file)


;; taken from https://github.com/bdd/.emacs.d/blob/master/bdd-defuns.el
(defun kill-region-or-backward-kill-word (arg)
  "If mark is active kill the region else backward kill word.

Traditionally Unix uses `C-w' for backward kill word. Preserve Emacs default
of kill-region if the mark is active, otherwise fallback to `backward-kill-word'.
Also fix `backward-kill-word' so that it stops at whitespace."
  (interactive "p")

  (defun backward-kill-word-without-spaces (arg)
    "Wrap backward-kill-word to swallow spaces separate from words."

    (if (looking-back "\\s-+") ; whitespace
        (kill-region (point)
                     (progn
                       (re-search-backward "\\S-") ; not whitespace
                       (forward-char 1)
                       (point)))
      (backward-kill-word arg)))

  (if mark-active
      (kill-region (point) (mark))
    (backward-kill-word-without-spaces arg)))

(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)


;; taken from http://stackoverflow.com/a/998472
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))
      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))
      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-d") 'duplicate-line)

;;; init.el ends here
