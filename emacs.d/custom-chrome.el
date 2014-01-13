(provide 'custom-chrome)

;; Remove unneeded chrome

(menu-bar-mode -1)

;; Config powerline (currently disabled as I can't make its symbols work)
(require 'powerline)
(require 'powerline-themes)

(defun powerline-personalized ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face0-bold (if active 'powerline-active0-bold 'powerline-inactive0-bold))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face1-bold (if active 'powerline-active1-bold 'powerline-inactive1-bold))
                          (face1-warning (if active 'powerline-active1-warning 'powerline-inactive1-warning))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-buffer-id face0 'l)
                                     (powerline-raw " " face0 'l)
                                     (funcall separator-left face0 face1)
                                     (powerline-major-mode face1-bold 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (if (buffer-modified-p)
                                         (progn
                                           (funcall separator-left face2 face1-warning)
                                           (powerline-raw " Modified" face1-warning 'r)
                                        ;(funcall separator-right face2 face1)
                                           )
                                       (funcall separator-right face1 face0))
                                     (powerline-raw " %6p" face0 'l)
                                     (powerline-raw "%4l" face0-bold 'l)
                                     (powerline-raw ":%3c " face0 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-personalized)

;; Manual modeline

;; (setq my-modeline-prefix
;;       '(:eval (propertize (if (buffer-modified-p) " Mod " "     ")
;;                           'face font-lock-warning-face)))
;; (setq my-modeline-buffer-name
;;       '(:eval (propertize "%b" 'face 'font-lock-keyword-face)))
;; (setq my-modeline-mode
;;       '(:eval (propertize "%m " 'face 'font-lock-string-face)
;;               minor-mode-alist))
;; (setq my-modeline-line
;;       '(:eval (propertize "%02l")))
;; (setq my-modeline-column
;;       '(:eval (propertize "%02c")))
;; (setq my-modeline-position
;;       '(:eval (propertize "%p")))
;; (setq my-modeline-size
;;       '(:eval (propertize "%I")))
;; (setq my-modeline-edit-state
;;       '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
;;                           'face 'font-lock-preprocessor-face)))
;; (setq my-modeline-file-state
;;       '(:eval (when buffer-read-only
;;                  (concat ","
;;                          (propertize "RO" 'face 'font-lock-type-face)))))

;; (setq-default mode-line-format
;;       (list
;;        my-modeline-prefix
;;        my-modeline-buffer-name
;;        " ("
;;        my-modeline-line
;;        " : "
;;        my-modeline-column
;;        ") ["
;;        my-modeline-position
;;        "/"
;;        my-modeline-size
;;        "] ["
;;        my-modeline-edit-state
;;        my-modeline-file-state
;;        "] "
;;        ))
