;; Packages I've imported from other peoples' github repos.

(provide 'packages)

;; helper utils
(defun add-to-load-path (file)
  (when (file-exists-p file)
	(add-to-list 'load-path file)))

(defun add-to-custom-themes (file)
  (when (file-exists-p file)
	(add-to-list 'custom-theme-load-path file)))

;; TODO: debug
(defun package-load-path (relative-path)
  (let ((path (concat default-directory relative-path)))
	(add-to-load-path path)))

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
   (change-theme 'color-theme-solarized-light 'color-theme-solarized-light)))

;; git-emacs
(add-to-load-path "~/.emacs.d/git-emacs")
(require 'git-emacs)

;; tuareg
(add-to-list 'load-path "~/.emacs.d/tuareg-mode")
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu" "Configuration of imenu for tuareg" t)
(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(add-to-list 'auto-mode-alist '("\\.ml[ily]?$" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.topml$" . tuareg-mode))

;; highlight-sexp
(add-to-load-path "~/.emacs.d/highlight-sexp/")
;(require 'highlight-sexp)
;(add-hook 'lisp-mode-hook 'highlight-sexp-mode)
;(add-hook 'emacs-lisp-mode-hook 'highlight-sexp-mode)

;; highlight-parenthesis
(add-to-load-path "~/.emacs.d/highlight-parentheses/")
(require 'highlight-parentheses)
(add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode)
