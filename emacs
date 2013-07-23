#! /bin/emacs
;; Emacs config file (~/.emacs)

;; Foreign github packages
(add-to-list 'load-path "~/.emacs.d/")
(require 'packages)

;; Put backup buffers somewhere NOT annoying
(defconst emacs-tmp-dir
  (format "/%s/%s-%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
  `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
  `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
  emacs-tmp-dir)

;; emacs-x config
(when window-system
  ;; Default geometry
  (set-frame-size (selected-frame) 104 35)

  ;; No left fringe
  (add-to-list 'initial-frame-alist '(left-fringe . 0)))

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
(setq linum-format "%3d ")
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
