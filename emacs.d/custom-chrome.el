(provide 'custom-chrome)

;; Remove unneeded chrome

(menu-bar-mode -1)
(tool-bar-mode -1)

;; Use hl-mode

(global-hl-line-mode 1)

;; XEmacs

(setq-default cursor-type 'bar)

;; Config powerline
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
                          (separator-left (intern (format "powerline-%s-left"
                                                          powerline-default-separator)))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-buffer-id face0 'l)
                                     (powerline-raw " " face0 'l)
                                     ;; (funcall separator-left face0 face1)
                                     (powerline-major-mode face1-bold 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     ;; (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (if (buffer-modified-p)
                                         (progn
                                           ;; (funcall separator-left face2 face1-warning)
                                           (powerline-raw " Modified" face1-warning 'r)
                                           ;; (funcall separator-right face2 face1)
                                           )
                                       ;; (funcall separator-right face1 face0)
                                       (powerline-raw " " face1 'r))
                                     (powerline-raw " %6p" face0 'l)
                                     (powerline-raw "%4l" face0-bold 'l)
                                     (powerline-raw ":%3c " face0 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-personalized)
