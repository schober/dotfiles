;; Customization of editing behavior
(provide 'custom-editing)

;; Start in text-mode, not fundamental-mode
(setq initial-major-mode 'text-mode)

;; We much prefer whole line kills
(setq kill-whole-line t)

;; When transiently marking, typing any letter deletes the selection first.
(delete-selection-mode 1)

;; Screw the default save-buffers-kill-terminal
(defun confirmationless-save-buffers-kill-terminal ()
  "Save modified buffers, then kill the current frame (and possibly Emacs) unconditionally."
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

;; Screw yes-or-no-p: we only need a single char to answer
(defalias 'yes-or-no-p 'y-or-n-p)

;; I want to be able to reload emacs's configs
(defun reload-emacs ()
  (interactive)
  (load-file "~/.emacs"))

;; Put backup buffers somewhere NOT annoying
(defconst emacs-tmp-dir
  (format "/%s/%s-%s/" temporary-file-directory "emacs" (user-uid)))

(setq backup-directory-alist
  `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
  `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix
      emacs-tmp-dir)
