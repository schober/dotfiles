#! /bin/emacs
(provide 'custom-keyboard)

(require 'packages)

;;;; Keyboard input remappings ;;;;

;; C-<arrow>
(define-key input-decode-map "\e[1;5A" (kbd "C-<up>"))
(define-key input-decode-map "\e[1;5B" (kbd "C-<down>"))
(define-key input-decode-map "\e[1;5C" (kbd "C-<right>"))
(define-key input-decode-map "\e[1;5D" (kbd "C-<left>"))

;; M-<arrow>
(define-key input-decode-map "\e[1;3A" (kbd "M-<up>"))
(define-key input-decode-map "\e[1;3B" (kbd "M-<down>"))
(define-key input-decode-map "\e[1;3C" (kbd "M-<right>"))
(define-key input-decode-map "\e[1;3D" (kbd "M-<left>"))

;; C-S-<arrow>
(define-key input-decode-map "\e[1;6A" (kbd "C-S-<up>"))
(define-key input-decode-map "\e[1;6B" (kbd "C-S-<down>"))
(define-key input-decode-map "\e[1;6C" (kbd "C-S-<right>"))
(define-key input-decode-map "\e[1;6D" (kbd "C-S-<left>"))

;; M-S-<arrow>
(define-key input-decode-map "\e[1;4A" (kbd "M-S-<up>"))
(define-key input-decode-map "\e[1;4B" (kbd "M-S-<down>"))
(define-key input-decode-map "\e[1;4C" (kbd "M-S-<right>"))
(define-key input-decode-map "\e[1;4D" (kbd "M-S-<left>"))

;; C-M-<arrow>
(define-key input-decode-map "\e[1;8A" (kbd "C-M-<up>"))
(define-key input-decode-map "\e[1;8B" (kbd "C-M-<down>"))
(define-key input-decode-map "\e[1;8C" (kbd "C-M-<right>"))
(define-key input-decode-map "\e[1;8D" (kbd "C-M-<left>"))

;; Home/End
(define-key input-decode-map "\e[4~" (kbd "<end>"))

;; We use C-] as an escape character for a "control" sequence (like meta)
(global-unset-key "\C-]")

;; C-\ (which I want to reserve for other uses)
(global-unset-key "\C-\\")

;; Workaround for M-[ (which is used as the xterm CSI) - we escape with C-]
(define-key input-decode-map (kbd "C-] M-[") (kbd "M-["))

;; Bind "C-<key>" for keys which we work around with "C-] <key>"
(dolist (key '("DEL" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "(" ")"))
  (let ((from-key (concat "C-] " key))
        (to-key (concat "C-" key)))
    (define-key input-decode-map (kbd from-key) (kbd to-key))))

;;;; Control remappings ;;;;

;; Global bindings
(dolist (global-key-rebinding
         `((,(kbd "C-<right>") . forward-sentence)
           (,(kbd "C-<left>") . backward-sentence)
           (,(kbd "M-<up>") . beginning-of-buffer)
           (,(kbd "M-<down>") . end-of-buffer)
           (,(kbd "M-{") . switch-to-prev-buffer)
           (,(kbd "M-}") . switch-to-next-buffer)
           (,(kbd "C-DEL") . backward-kill-sentence)
           ))
  (global-set-key (car global-key-rebinding)
		  (cdr global-key-rebinding)))

;; Lisp mode bindings
(dolist (lisp-mode-pair
	 '((emacs . emacs-lisp-mode-map)
	   (emacs . lisp-mode-map)
	   (paredit . paredit-mode-map)))
  (eval-after-load (car lisp-mode-pair)
    `(progn
       (define-key ,(cdr lisp-mode-pair) (kbd "C-<right>") 'forward-sexp)
       (define-key ,(cdr lisp-mode-pair) (kbd "C-<left>") 'backward-sexp)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-<up>") 'beginning-of-buffer)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-<down>") 'end-of-buffer)
       (define-key ,(cdr lisp-mode-pair) (kbd "C-DEL") 'backward-kill-sexp)
       )))

(define-key paredit-mode-map (kbd "RET") 'newline-and-indent)
(define-key paredit-mode-map (kbd "C-\\") 'paredit-convolute-sexp)
(define-key paredit-mode-map (kbd "C-] [") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-] ]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] {") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] }") 'paredit-backward-barf-sexp)
