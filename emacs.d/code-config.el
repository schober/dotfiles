;; Configuration relating to coding
(provide 'code-config)

;; Enable syntax highlighting
(setq global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Line numbers to the right
(global-linum-mode 1)

;; Column numbers too!
(setq column-number-mode t)

;; We much prefer whole line kills
(setq kill-whole-line t)

;; Set language-specific features
(setq c-default-style "linux"
      c-basic-offset 4
      indent-tabs-mode nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Display whitespace
(setq whitespace-style '(face trailing tabs tab-mark)
      whitespace-display-mappings '((tab-mark ?\t [?\xBB ?\t])))

;; Mode inclusion
(add-hook 'clojure-mode-hook 'eldoc-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Utilities for converting between different capitalization canonicalizations
(defun camelcase  (s) (mapconcat 'capitalize (split-name s) ""))
(defun underscore (s) (mapconcat 'downcase   (split-name s) "_"))
(defun dasherize  (s) (mapconcat 'downcase   (split-name s) "-"))
(defun colonize   (s) (mapconcat 'capitalize (split-name s) "::"))

(defun camelize (s)
  "Convert under_score string S to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-string s "_")) ""))

(defun camelize-previous-snake (&optional beg end)
  "Camelize the previous snake cased string.

    If transient-mark-mode is active and a region is activated,
    camelize the region."
  (interactive "r")
  (unless (and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
    (setq end (point)
          beg (+ (point) (skip-chars-backward "[:alnum:]_"))))
  (save-excursion
    (let ((c (camelize (buffer-substring-no-properties beg end))))
      (delete-region beg end)
      (goto-char beg)
      (insert c))))
