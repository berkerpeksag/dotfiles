(add-to-list 'load-path "~/.emacs.d/")
; Modes path
(add-to-list 'load-path "~/.emacs.d/modes/")

(byte-recompile-directory "." 0)

(load "modes-init")
(load "variables")
(load "keys")

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

; Save the old theme for now.
;(require 'color-theme)
;(setq color-theme-is-global t)
;(color-theme-clarity)
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized/")
(require 'color-theme-solarized)

;; Taken from http://stackoverflow.com/a/2706660/57823
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
 "Prevent annoying \"Active processes exist\" query when you quit Emacs."
 (flet ((process-list ())) ad-do-it))
