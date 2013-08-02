;; Manually imported packages
;; helper utils

(provide 'util)

(defun add-to-load-path (file)
  (when (file-exists-p file)
	(add-to-list 'load-path file)))
(provide 'add-to-load-path)

(defun add-to-custom-themes (file)
  (when (file-exists-p file)
	(add-to-list 'custom-theme-load-path file)))
(provide 'add-to-custom-themes)

(defun load-package (relative-path &optional package-name)
  (add-to-load-path (concat (file-name-directory load-file-name) relative-path))
  (if (null package-name)
	  (require (intern relative-path))
	(require package-name)))
(provide 'load-package)
