(provide 'custom-chrome)

;; Remove unneeded chrome
(menu-bar-mode -1)
(tool-bar-mode -1)

;; No useless splash screen
(setq inhibit-splash-screen t)

;; Scratch buffer starts empty
(setq initial-scratch-message "")

;; Enable line numbers in every buffer
(global-linum-mode 1)

;; Highlight the current line (emphasizes the cursor)
(global-hl-line-mode 1)

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
                          ;; problems: powerline-active1 powerline-active2 powerline-inactive1-warning
                          (face1 (if active 'powerline-inactive1 'powerline-inactive1)) ;; wtf
                          (face1-bold (if active 'powerline-active1-bold 'powerline-inactive1-bold))
                          (face1-warning (if active 'powerline-active1-warning 'powerline-inactive1)) ;; wtf
                          (face2 (if active 'powerline-inactive2 'powerline-inactive2)) ;; wtf
                          (separator-left (intern (format "powerline-%s-left"
                                                          powerline-default-separator)))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-buffer-id face0-bold 'l)
                                     (powerline-raw " " face0 'l)
                                     (funcall separator-left face0 face1)
                                     (powerline-major-mode face1-bold 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (funcall separator-right face2 (if (buffer-modified-p) face1-warning face1))
                                     (if (buffer-modified-p)
                                         (powerline-raw " Modified" face1-warning 'r)
                                       (powerline-raw " " face1 'r))
                                     (funcall separator-right (if (buffer-modified-p) face1-warning face1) face0)
                                     (powerline-raw " %6p" face0 'l)
                                     (powerline-raw "%4l" face0-bold 'l)
                                     (powerline-raw ":%3c " face0 'r))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))
(powerline-personalized)

;; XEmacs-specific Configuration
(when window-system
  ;; Default geometry
  (set-frame-size (selected-frame) 138 38)

  ;; No left fringe
  (add-to-list 'initial-frame-alist '(left-fringe . 0))

  ;; Bar cursor, not block
  (setq-default cursor-type 'bar)

  ;; Default font: Inconsolata
  ;; (set-face-attribute 'default nil :font "Inconsolata")
  )

;; When looking at buffers with ido, hide all *system-buffers* except *scratch*.
(defvar ido-ignore-buffer-whitelist '("*scratch*"))
(defun ido-ignore-buffer-most-stars (name)
  (and (string-match-p "^*" name)
       (not (member name ido-ignore-buffer-whitelist))))
