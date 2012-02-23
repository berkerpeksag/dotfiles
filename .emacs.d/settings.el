;;; Hooks
(defun add-mode-line-dirtrack ()
  "When editing a file, show the last 2 directories of the current path in the mode line."
  (add-to-list 'mode-line-buffer-identification
	       '(:eval (substring default-directory
				  (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))

(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;;; Custom keys
(set-default-font "Monospace-11")

(setq frame-title-format '("emacs@" system-name ": %b %+%+ %f"))

;;; See: http://www.emacswiki.org/cgi-bin/wiki/CopyAndPaste#toc5
(setq x-select-enable-clipboard t)
(setq inter-program-paste-function 'x-cut-buffer-or-selection-value)


(custom-set-variables
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-empty-lines t)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)