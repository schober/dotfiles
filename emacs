;; Emacs config file (~/.emacs)

;; ;; Uncomment to profile startup
;; (require 'benchmark-init)

(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
(require 'packages)


;; I want to be able to reload emacs's configs
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

;; Emacs-managed config

(defface powerline-inactive0 '((t (:inherit default))) "")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-requires 4)
 '(c-basic-offset 2)
 '(custom-safe-themes
   (quote
    ("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(default-frame-alist (quote ((width . 134) (height . 40))))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(enable-recursive-minibuffers t)
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin13.0.0" "/usr/local/bin" "~/bin")))
 '(fill-column 100)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(ido-ignore-buffers (quote ("\\` " ido-ignore-buffer-most-stars)))
 '(indent-tabs-mode nil)
 '(ispell-program-name "ispell")
 '(linum-format "%3d ")
 '(linum-mode (quote dynamic) t)
 '(org-agenda-files (quote ("~/Google Drive/Org/")))
 '(org-catch-invisible-edits (quote show-and-error))
 '(org-log-done (quote time))
 '(org-special-ctrl-k t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (jsonrpc eglot auto-compile string-inflection benchmark-init epl magit json-mode idle-highlight)))
 '(powerline-default-separator (quote slant))
 '(sentence-end-double-space nil)
 '(sh-learn-basic-offset t)
 '(solarized-broken-srgb nil)
 '(tab-width 2)
 '(text-mode-hook
   (quote
    (turn-on-flyspell text-mode-hook-identify toggle-word-wrap)))
 '(undo-limit 2048576)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(powerline-active1 ((t (:inherit powerline-inactive1))))
 '(powerline-active2 ((t (:inherit powerline-inactive2)))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(redraw-display)
