(provide 'custom-ac)

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(custom-set-variables
 '(ac-ispell-requires 4))

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)
     (require 'go-autocomplete)))

(defun my/enable-ac-ispell ()
  (add-to-list 'ac-sources 'ac-source-ispell))

(add-hook 'git-commit-mode-hook 'my/enable-ac-ispell)
(add-hook 'mail-mode-hook 'my/enable-ac-ispell)
(add-hook 'text-mode-hook 'my/enable-ac-ispell)
