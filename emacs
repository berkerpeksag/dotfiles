; -*- mode: emacs-lisp;-*-
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(whitespace-mode 0)
(ido-mode 1)
(ido-everywhere 1)
(global-hl-line-mode 1)
(global-display-fill-column-indicator-mode 1)
(cua-mode t) ; Normal cut, copy, paste, and undo.
(global-auto-revert-mode 1)

(setq-default
              inhibit-startup-screen t
              make-backup-files nil
              tab-width 4
              fill-column 120
              indent-tabs-mode nil)

(setq
      cua-keep-region-after-copy t
      ;; Use left alt/option as meta and the right one for stock Apple stuff
      ns-alternate-modifier 'meta
      ns-right-alternate-modifier 'none
      ;; #FOO# files
      kill-buffer-delete-auto-save-files t
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      auto-save-timeout 5 ; seconds
      ;; FOO~ files
      make-backup-files nil
      ;; .#FOO files
      create-lockfiles nil
      require-final-newline t
      file-preserve-symlinks-on-save t
      vc-follow-symlinks nil)

;; Custom key bindings for builtin functions.
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c b") 'end-of-buffer)
(global-set-key (kbd "<f5>") 'revert-buffer-quick)

;; duplicate-line is available in Emacs 29.1, but Debian only has 28.2.
(unless (fboundp 'duplicate-line)
  (defun duplicate-line ()
    (interactive)
    (let ((current-line (thing-at-point 'line t)))
      (save-excursion
        (end-of-line)
        (insert "\n" current-line)))))

(defun duplicate-line-and-move-cursor ()
  (interactive)
  (let ((current-column (current-column)))
    (call-interactively 'duplicate-line)
    (forward-line 1)
    (move-to-column current-column)))

(global-set-key (kbd "C-.") 'duplicate-line-and-move-cursor)

(defun get-indentation-width ()
  "Determine the appropriate indentation width based on the current major mode."
  (cond
   ((eq major-mode 'python-mode)
    (or (bound-and-true-p python-indent-offset) tab-width))
   ((eq major-mode 'js-mode)
    (or (bound-and-true-p js-indent-level) tab-width))
   ((eq major-mode 'c-mode)
    (or (bound-and-true-p c-basic-offset) tab-width))
   ((eq major-mode 'html-mode)
    (or (bound-and-true-p sgml-basic-offset) tab-width))
   ((eq major-mode 'css-mode)
    (or (bound-and-true-p css-indent-offset) tab-width))
   (t tab-width)))

(defun normal-indent-region (begin end)
  "Indent the selected region or current line by the appropriate indentation width."
  (interactive "r")
  (let* ((indent-size (get-indentation-width))
         (deactivate-mark nil))
    (if (use-region-p)
        (indent-rigidly begin end indent-size)
      (indent-rigidly (line-beginning-position) (line-end-position) indent-size))))

(defun normal-dedent-region (begin end)
  "Dedent the selected region or current line by the appropriate indentation width."
  (interactive "r")
  (let* ((indent-size (get-indentation-width))
         (deactivate-mark nil))
    (if (use-region-p)
        (indent-rigidly begin end (- indent-size))
      (indent-rigidly (line-beginning-position) (line-end-position) (- indent-size)))))

(global-set-key (kbd "<tab>") 'normal-indent-region)
(global-set-key (kbd "<backtab>") 'normal-dedent-region)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)
