;; Emacs config file (~/.emacs)

;; Initialize packages
(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Encapsulated setup
(add-to-list 'load-path (expand-file-name "~/.emacs.d/init"))
;; (require 'benchmark-init)  ;; Uncomment to profile startup.
(require 'packages)
(require 'custom-keyboard)
(require 'code-config)
(require 'custom-chrome)

(require 'util)

;; When using emacsclient, reload the keybindings
(defun reload-key-bindings ()
  (interactive)
  (load-file "~/.emacs.d/init/custom-keyboard.el"))
(add-hook 'server-visit-hook 'reload-key-bindings)

;; Scratch should be empty and start in text-mode
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;; No useless splash screen
(setq inhibit-splash-screen t)

;; Screw the default save-buffers-kill-terminal
(defun confirmationless-save-buffers-kill-terminal ()
  "Save modified buffers, then kill the current frame (and possibly emacs) unconditionally."
  (interactive)
  (save-some-buffers nil t)
  (if (frame-parameter (selected-frame) 'client)
      (delete-frame)
    (kill-emacs)))

(global-set-key (kbd "C-x C-c") 'confirmationless-save-buffers-kill-terminal)
(global-set-key (kbd "C-x c") 'confirmationless-save-buffers-kill-terminal)
(global-set-key (kbd "C-c C-x c") 'confirmationless-save-buffers-kill-terminal)
(global-set-key (kbd "C-c C-x C-c") 'confirmationless-save-buffers-kill-terminal)

;; Quick and painless buffer killing
(defun confirmationless-save-and-kill-buffer ()
  "Offer to save the current buffer, then kill it without further confirmation."
  (interactive)
  (let ((my-current-buffer (current-buffer)))
    (save-some-buffers nil (lambda () (eq (current-buffer)
                                          my-current-buffer))))
  (set-buffer-modified-p nil)
  (kill-this-buffer))

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
 '(enable-recursive-minibuffers t)
 '(exec-path
   (quote
    ("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/usr/local/Cellar/emacs/24.3/libexec/emacs/24.3/x86_64-apple-darwin13.0.0" "/usr/local/bin" "~/bin")))
 '(fill-column 100)
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(indent-tabs-mode nil)
 '(ispell-program-name "ispell")
 '(linum-format "%3d ")
 '(linum-mode (quote dynamic) t)
 '(org-startup-folded nil)
 '(package-selected-packages
   (quote
    (auto-compile string-inflection benchmark-init epl magit json-mode idle-highlight)))
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
