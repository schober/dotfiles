#! /bin/emacs
(provide 'custom-keyboard)

(require 'packages)

;;;; Keyboard input remappings ;;;;

;; C-<arrow>
(define-key input-decode-map "\e[1;5A" [(control up)])
(define-key input-decode-map "\e[1;5B" [(control down)])
(define-key input-decode-map "\e[1;5C" [(control right)])
(define-key input-decode-map "\e[1;5D" [(control left)])

;; M-<arrow>
(define-key input-decode-map "\e[1;4A" [(meta up)])
(define-key input-decode-map "\e[1;4B" [(meta down)])
(define-key input-decode-map "\e[1;4C" [(meta right)])
(define-key input-decode-map "\e[1;4D" [(meta left)])
(define-key input-decode-map "\e[1;3A" [(meta up)])
(define-key input-decode-map "\e[1;3B" [(meta down)])
(define-key input-decode-map "\e[1;3C" [(meta right)])
(define-key input-decode-map "\e[1;3D" [(meta left)])

;; M-S-<arrow>
(define-key input-decode-map "\e[1;10A" [(meta shift up)])
(define-key input-decode-map "\e[1;10B" [(meta shift down)])
(define-key input-decode-map "\e[1;10C" [(meta shift right)])
(define-key input-decode-map "\e[1;10D" [(meta shift left)])

;; C-M-<arrow>
(define-key input-decode-map "\e[1;8A" [(control meta up)])
(define-key input-decode-map "\e[1;8B" [(control meta down)])
(define-key input-decode-map "\e[1;8C" [(control meta right)])
(define-key input-decode-map "\e[1;8D" [(control meta left)])

;; Home/End
(define-key input-decode-map "\e[4~" [end])

;;;; Control remappings ;;;;

;; Global bindings
(dolist (global-key-rebinding
         `((,(kbd "C-<right>") . forward-sentence)
           (,(kbd "C-<left>") . backward-sentence)
           (,(kbd "M-<up>") . beginning-of-buffer)
           (,(kbd "M-<down>") . end-of-buffer)
           (,(kbd "M-{") . switch-to-prev-buffer)
           (,(kbd "M-}") . switch-to-next-buffer)))
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
       (define-key ,(cdr lisp-mode-pair) (kbd "M-{") 'switch-to-prev-buffer)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-}") 'switch-to-next-buffer))))

(define-key paredit-mode-map (kbd "RET") 'newline-and-indent)
