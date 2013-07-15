#! /bin/emacs
;; Emacs config file (~/.emacs)
;; Configuring emacs is hard!

;; Little util here.
(defun add-to-load-path (file)
  (when (file-exists-p file)
	(add-to-list 'load-path file)))

;; Daytime/nighttime theme changer
(add-to-load-path "~/.emacs.d/theme-changer")
(setq calendar-location-name "New York, NY") 
(setq calendar-latitude 40.67)
(setq calendar-longitude -73.94)

;; Load themes, default to zenburn
(cond
  ((>= emacs-major-version 24)
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
   (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
   (require 'theme-changer)
   (change-theme 'solarized-light 'solarized-dark))
  ((= emacs-major-version 23)
   (add-to-list 'load-path "~/.emacs.d/themes")
   (add-to-list 'load-path "~/.emacs.d/themes/solarized")
   (require 'color-theme-solarized)
   (require 'color-theme-zenburn)
   (setq theme-changer-mode "color-theme")
   (change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)
))

;; Put backup buffers somewhere NOT annoying
(defconst emacs-tmp-dir
  (format "/%s/%s-%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
  `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
  `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
  emacs-tmp-dir)

;; Set up slime
(when (file-exists-p "~/.slime")
  (add-to-list 'load-path "~/.slime")
  (setq inferior-lisp-program "sbcl")
  (require 'slime)
  (slime-setup '(slime-repl slime-fancy slime-asdf)))

;; Enable syntax highlighting
(setq global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Line numbers to the right
(global-linum-mode 1)
(setq linum-format "%d ")
(custom-set-variables '(linum-mode 'dynamic))

;; Column numbers too!
(setq column-number-mode t)

;; We much prefer whole line kills
(setq kill-whole-line t)

;; No useless splash screen
(setq inhibit-splash-screen t)

;; Set language-specific features
(setq c-default-style "linux"
      c-basic-offset 4)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(tab-width 4)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
