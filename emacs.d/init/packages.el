;; Imported packages and systems
(provide 'packages)

;; Use melpa-stable only. melpa-latest has too many versions and elpa has
;; nothing.
(dolist (archive '(("melpa-stable" . "https://stable.melpa.org/packages/")))
  (add-to-list 'package-archives archive t))
(package-initialize)

;; auto-compile (ensures all .el files are compiled when loaded/saved)
(unless (package-installed-p 'auto-compile)
  (package-refresh-contents)
  (package-install 'auto-compile))
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Helpers for the rest of this method
(require 'util)
(require 'package)

;; mode-mode
(require 'mode-mode)

;; powerline
(add-to-load-path "~/.emacs.d/powerline")
(require 'powerline)

;; Load themes, default to solarized
(cond
 ((>= emacs-major-version 24)
  (add-to-custom-themes "~/.emacs.d/themes/solarized")
  (load-theme 'solarized t))
 ((= emacs-major-version 23)
  (add-to-load-path "~/.emacs.d/themes/solarized")
  (require 'color-theme)
  (require 'color-theme-solarized)
  (color-theme-solarized-light)))

;; paredit mode
(load-package "paredit")
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(dolist (mode '(emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook ielm-mode-hook
                lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook mode #'enable-paredit-mode))
;; Fix deletion while selecting
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-forward-delete 'delete-selection 'supersede)

;; Set up slime
(when (file-exists-p "~/.slime")
  (load-package ".slime" 'slime)
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-repl slime-fancy slime-asdf)))

;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Org mode
(require 'org)

;; Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; UI and Keyboard customization
(require 'custom-editing)
(require 'custom-keyboard)
(require 'custom-chrome)
