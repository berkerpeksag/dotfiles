(tool-bar-mode 0)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)
(whitespace-mode 0)
(ido-mode 1)
(ido-everywhere 1)

(setq-default inhibit-splash-screen t
              make-backup-files nil
              tab-width 4
              indent-tabs-mode nil)

(setq
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
      create-lockfiles nil)

(defun duplicate-line-and-move-cursor ()
  (interactive)
  (let ((current-column (current-column)))
    (call-interactively 'duplicate-line)
    (forward-line 1)
    (move-to-column current-column)))

(global-set-key (kbd "C-.") 'duplicate-line-and-move-cursor)

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file))

(global-set-key (kbd "C-c C-l") 'reload-init-file)

;; To separate custom-set-variables from actual configuration, do:
;; (setq custom-file "")
;; (load custom-file)
