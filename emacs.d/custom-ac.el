(provide 'custom-ac)

(eval-after-load "auto-complete"
  '(progn
     (ac-ispell-setup)))

(defun my/enable-ac-ispell ()
  (add-to-list 'ac-sources 'ac-source-ispell))

(add-hook 'git-commit-mode-hook 'my/enable-ac-ispell)
(add-hook 'mail-mode-hook 'my/enable-ac-ispell)
