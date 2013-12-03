#! /bin/emacs
;; Emacs config file (~/.emacs)

;; Encapsulated setup
(add-to-list 'load-path "~/.emacs.d")
(require 'custom-keyboard)
(require 'packages)
(require 'code-config)

(require 'util)

;; Scratch should be empty and start in text-mode
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; No useless splash screen
(setq inhibit-splash-screen t)

;; Screw the default save-buffers-kill-terminal
(defun promptless-save-buffers-kill-terminal ()
  "save some buffers, then exit unconditionally"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(global-set-key (kbd "C-x C-c") 'promptless-save-buffers-kill-terminal)
(global-set-key (kbd "C-x c") 'promptless-save-buffers-kill-terminal)

;; Emacs-managed config
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2 t)
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(linum-mode (quote dynamic) t)
 '(sentence-end-double-space nil)
 '(tab-width 4)
 '(vc-follow-symlinks t))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "brightblack" :foreground "brightblue" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1 :width normal :foundry "default" :family "default"))))
;;  '(error ((t (:foreground "red" :inverse-video nil :underline nil :slant normal :weight bold))))
;;  '(font-lock-builtin-face ((t (:foreground "green" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face :foreground "brightgreen" :inverse-video nil :underline nil :slant italic :weight normal))))
;;  '(font-lock-comment-face ((t (:foreground "brightgreen" :inverse-video nil :underline nil :slant italic :weight normal))))
;;  '(font-lock-constant-face ((t (:foreground "cyan" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(font-lock-doc-face ((t (:inherit font-lock-comment-face :foreground "brightgreen" :inverse-video nil :underline nil :slant italic :weight normal))))
;;  '(font-lock-function-name-face ((t (:foreground "blue" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(font-lock-keyword-face ((t (:foreground "green" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(font-lock-string-face ((t (:foreground "cyan" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(font-lock-type-face ((t (:foreground "yellow" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(font-lock-variable-name-face ((t (:foreground "blue" :inverse-video nil :underline nil :slant normal :weight normal))))
;;  '(linum ((t (:inherit (shadow default) :background "black" :foreground "brightgreen"))))
;;  '(sh-quoted-exec ((t (:foreground "red"))))
;;  '(whitespace-tab ((t (:background "black" :foreground "brightgreen"))) t)
;;  '(whitespace-trailing ((t (:background "red" :foreground "#d70000" :inverse-video nil :underline nil :slant normal :weight normal))) t))

;; emacs-x config
(when window-system
  ;; Default geometry
  (set-frame-size (selected-frame) 104 35)
  ;; No left fringe
  (add-to-list 'initial-frame-alist '(left-fringe . 0)))

;; Put backup buffers somewhere NOT annoying
(defconst emacs-tmp-dir
  (format "/%s/%s-%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
  `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
  `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
  emacs-tmp-dir)
