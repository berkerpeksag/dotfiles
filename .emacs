(add-to-list 'load-path "~/.emacs.d/")

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(require 'python-mode)
(add-hook 'python-mode-hook
    (lambda ()
	    (set-variable 'py-indent-offset 4)
	    ;(set-variable 'py-smart-indentation nil)
	    (set-variable 'indent-tabs-mode nil)
	    (define-key py-mode-map (kbd "RET") 'newline-and-indent)
	    ;(define-key py-mode-map [tab] 'yas/expand)
	    ;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
	    (smart-operator-mode-on)))

(setq ropemacs-enable-autoimport t)

(defun prefix-list-elements (list prefix)
  (let (value)
    (nreverse
     (dolist (element list value)
      (setq value (cons (format "%s%s" prefix element) value))))))

(defvar ac-source-rope
  '((candidates
     . (lambda ()
         (prefix-list-elements (rope-completions) ac-target))))
            "Source for Rope")

(defun ac-python-find ()
  "Python `ac-find-function'."
  (require 'thingatpt)
  (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
    (if (null symbol)
        (if (string= "." (buffer-substring (- (point) 1) (point)))
            (point)
          nil)
      symbol)))

(defun ac-python-candidate ()
  "Python `ac-candidates-function'"
  (let (candidates)
    (dolist (source ac-sources)
      (if (symbolp source)
          (setq source (symbol-value source)))
      (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
             (requires (cdr-safe (assq 'requires source)))
             cand)
        (if (or (null requires)
                (>= (length ac-target) requires))
            (setq cand
                  (delq nil
                        (mapcar (lambda (candidate)
                                  (propertize candidate 'source source))
                                (funcall (cdr (assq 'candidates source)))))))
        (if (and (> ac-limit 1)
                 (> (length cand) ac-limit))
            (setcdr (nthcdr (1- ac-limit) cand) nil))
        (setq candidates (append candidates cand))))
    (delete-dups candidates)))

(add-hook 'python-mode-hook
          (lambda ()
                 (auto-complete-mode 1)
                 (set (make-local-variable 'ac-sources)
                      (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
                 (set (make-local-variable 'ac-find-function) 'ac-python-find)
                 (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
                 (set (make-local-variable 'ac-auto-start) nil)))

;;Ryan's python specific tab completion
(defun ryan-python-tab ()
  ; Try the following:
  ; 1) Do a yasnippet expansion
  ; 2) Do a Rope code completion
  ; 3) Do an indent
  (interactive)
  (if (eql (ac-start) 0)
      (indent-for-tab-command)))

(defadvice ac-start (before advice-turn-on-auto-start activate)
  (set (make-local-variable 'ac-auto-start) t))

(defadvice ac-cleanup (after advice-turn-off-auto-start activate)
  (set (make-local-variable 'ac-auto-start) nil))

(define-key py-mode-map "\t" 'ryan-python-tab)

(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)
(provide 'init_python)

(require 'rvm)
(rvm-use-default)

(add-to-list 'load-path "~/.emacs.d/go-mode-load.el" t)
(require 'go-mode-load)

(require 'whitespace)
(global-set-key "\C-c_w" 'whitespace-mode)
(global-set-key "\C-c_t" 'whitespace-toggle-options)
(global-set-key "\C-c=w" 'global-whitespace-mode)
(global-set-key "\C-c=t" 'global-whitespace-toggle-options)

(defun add-mode-line-dirtrack ()
    "When editing a file, show the last 2 directories of the current path in the mode line."
    (add-to-list 'mode-line-buffer-identification 
                 '(:eval (substring default-directory 
                                    (+ 1 (string-match "/[^/]+/[^/]+/$" default-directory)) nil))))

(add-hook 'find-file-hook 'add-mode-line-dirtrack)

;;; Scala mode
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)

;;; Ensime
;;;(add-to-list 'load-path "~/ensime/elisp/")
;;;(require 'ensime)

;;; Google Emacs Navigation
(add-to-list 'load-path "~/.emacs.d/emacs-nav")
(require 'nav)

;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;;; Clojure
(add-to-list 'load-path "~/.emacs.d/clojure-mode.el")
(require 'clojure-mode)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

(setq frame-title-format '("emacs@" system-name ": %b %+%+ %f"))

(require 'linum)

(require 'cl)