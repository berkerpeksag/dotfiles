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
      whitespace-line-column fill-column
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
(global-set-key (kbd "C-c w") 'whitespace-mode)

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

(defun smart-indent-region ()
  "Indent region or current line based on context."
  (interactive)
  (if (use-region-p)
      (let* ((indent-size (get-indentation-width))
             (deactivate-mark nil))
        (indent-rigidly (region-beginning) (region-end) indent-size))
    (cond
     ;; On empty lines, insert tabs or call the default indentation function.
     ((looking-at "^[[:space:]]*$")
      (call-interactively 'indent-for-tab-command))
     ;; For non-empty lines, apply our custom indentation.
     (t
      (let ((indent-size (get-indentation-width)))
        (indent-rigidly (line-beginning-position) (line-end-position) indent-size))))))

(defun smart-dedent-region ()
  "Dedent region or current line based on context."
  (interactive)
  (if (use-region-p)
      (let* ((indent-size (get-indentation-width))
             (deactivate-mark nil))
        (indent-rigidly (region-beginning) (region-end) (- indent-size)))
    (let ((indent-size (get-indentation-width)))
      (indent-rigidly (line-beginning-position) (line-end-position) (- indent-size)))))

(defun setup-indent-keys ()
  (local-set-key (kbd "<tab>") 'smart-indent-region)
  (local-set-key (kbd "<backtab>") 'smart-dedent-region))

(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook 'setup-indent-keys))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)
