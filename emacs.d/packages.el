;; Packages I've imported from other peoples' github repos.
(provide 'packages)

(require 'util)

;; ELPA
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))


;; Daytime/nighttime theme changer
(add-to-load-path "~/.emacs.d/theme-changer")
(setq calendar-location-name "New York, NY") 
(setq calendar-latitude 40.67)
(setq calendar-longitude -73.94)

;; Load themes, default to solarized
(cond
  ((>= emacs-major-version 24)
   (add-to-custom-themes "~/.emacs.d/themes")
   (add-to-custom-themes "~/.emacs.d/themes/solarized")
   (require 'theme-changer)
   (change-theme 'solarized-light 'solarized-dark))
  ((= emacs-major-version 23)
   (add-to-load-path "~/.emacs.d/themes")
   (add-to-load-path "~/.emacs.d/themes/solarized")
   (require 'color-theme-solarized)
   (require 'color-theme-zenburn)
   (setq theme-changer-mode "color-theme")
   (change-theme 'color-theme-solarized-light 'color-theme-solarized-dark)))

;; git-emacs
(load-package "git-emacs")

;; tuareg
(add-to-list 'load-path "~/.emacs.d/tuareg-mode")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))

;; highlight-parenthesis
(load-package "highlight-parentheses")
(dolist (mode '(lisp-mode-hook emacs-lisp-mode-hook slime-mode))
  (add-hook mode 'highlight-parentheses-mode))

;; paredit mode
(load-package "paredit")
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
