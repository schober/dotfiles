;; Imported packages and systems
(provide 'packages)

;; Helpers for the rest of this method
(require 'util)
(require 'package)

;; mode-mode
(require 'mode-mode)

;; ELPA
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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

;; Autocomplete
(require 'custom-ac)

;; multiple-cursors
(add-to-load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)

;; tuareg
(add-to-list 'load-path "~/.emacs.d/tuareg-mode")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))

;; paredit mode
(load-package "paredit")
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(dolist (mode '(emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook ielm-mode-hook
                lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook
                slime-repl-mode-hook))
  (add-hook mode #'enable-paredit-mode))

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
(setq org-agenda-files '("~/Google Drive/Agenda"))

;; Ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
