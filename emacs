#! /bin/emacs
;; Emacs config file (~/.emacs)

;; Encapsulated setup
(add-to-list 'load-path "~/.emacs.d")
(require 'util)
(require 'packages)
(require 'custom-keyboard)

;; Put backup buffers somewhere NOT annoying
(defconst emacs-tmp-dir
  (format "/%s/%s-%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
  `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
  `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
  emacs-tmp-dir)

;; Scratch should be empty and start in text-mode
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; Screw the default save-buffers-kill-terminal
(defun promptless-save-buffers-kill-terminal ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'promptless-save-buffers-kill-terminal)
(global-set-key (kbd "C-x c") 'promptless-save-buffers-kill-terminal)

;; emacs-x config
(when window-system
  ;; Default geometry
  (set-frame-size (selected-frame) 104 35)

  ;; No left fringe
  (add-to-list 'initial-frame-alist '(left-fringe . 0)))

;; Set up slime
(when (file-exists-p "~/.slime")
  (load-package ".slime" 'slime)
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl slime-fancy slime-asdf)))

;; git-emacs
(add-to-load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)

;; Enable syntax highlighting
(setq global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Line numbers to the right
(global-linum-mode 1)
(setq linum-format "%3d ")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(linum-mode (quote dynamic) t)
 '(sentence-end-double-space nil)
 '(vc-follow-symlinks t))

;; Column numbers too!
(setq column-number-mode t)

;; We much prefer whole line kills
(setq kill-whole-line t)

;; No useless splash screen
(setq inhibit-splash-screen t)

;; Set language-specific features
(setq c-default-style "linux"
      c-basic-offset 4)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)
