;; Helper utils for emacs configuration

(provide 'util)

(defun add-to-load-path (file)
  (when (file-exists-p file)
	(add-to-list 'load-path file)))
(provide 'add-to-load-path)

(defun add-to-custom-themes (file)
  (when (file-exists-p file)
	(add-to-list 'custom-theme-load-path file)))
(provide 'add-to-custom-themes)

(defvar emacs-d-default-folder (expand-file-name "~/.emacs.d/"))

(defun load-package (emacs-d-relative-path &optional package-name)
  (add-to-load-path (concat emacs-d-default-folder emacs-d-relative-path))
  (if (null package-name)
      (require (intern (first (split-string emacs-d-relative-path "/"))))
    (require package-name)))
(provide 'load-package)
