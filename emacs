;; Emacs config file (~/.emacs)
;; Configuring emacs is hard!

;; Load themes, default to zenburn
(cond
  ((>= emacs-major-version 24)
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
   (load-theme 'zenburn t))
  ((= emacs-major-version 23)
   (add-to-list 'load-path "~/.emacs.d/themes/")
   (require 'color-theme-zenburn)
   (color-theme-zenburn)))

;; Set up slime
(when (file-exists-p "~/.slime")
  (progn
    (add-to-list 'load-path "~/.slime")
    (setq inferior-lisp-program "sbcl")
    (require 'slime)
    (slime-setup '(slime-repl))))

;; Enable syntax highlighting
(setq global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; We much prefer whole line kills
(setq kill-whole-line t)

(setq inhibit-splash-screen t)