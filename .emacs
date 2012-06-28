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

(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-clarity)

(require 'smooth-scrolling)

(custom-set-variables
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(indicate-empty-lines t)
 '(safe-local-variable-values (quote ((eval add-hook (quote write-file-hooks) (quote time-stamp)))))
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(tool-bar-mode nil))

;; Add newline at end of files
(setq require-final-newline t)
