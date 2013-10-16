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
(define-key input-decode-map "\e[1;3A" [(meta up)])
(define-key input-decode-map "\e[1;3B" [(meta down)])
(define-key input-decode-map "\e[1;3C" [(meta right)])
(define-key input-decode-map "\e[1;3D" [(meta left)])

;; C-S-<arrow>
(define-key input-decode-map "\e[1;6A" [(control shift up)])
(define-key input-decode-map "\e[1;6B" [(control shift down)])
(define-key input-decode-map "\e[1;6C" [(control shift right)])
(define-key input-decode-map "\e[1;6D" [(control shift left)])

;; M-S-<arrow>
(define-key input-decode-map "\e[1;4A" [(meta shift up)])
(define-key input-decode-map "\e[1;4B" [(meta shift down)])
(define-key input-decode-map "\e[1;4C" [(meta shift right)])
(define-key input-decode-map "\e[1;4D" [(meta shift left)])

;; C-M-<arrow>
(define-key input-decode-map "\e[1;8A" [(control meta up)])
(define-key input-decode-map "\e[1;8B" [(control meta down)])
(define-key input-decode-map "\e[1;8C" [(control meta right)])
(define-key input-decode-map "\e[1;8D" [(control meta left)])

;; Home/End
(define-key input-decode-map "\e[4~" [end])

;; C-[ and C-] (yes, I'm taking over this house)
(global-unset-key "\C-]")

;; C-\ (which I want to reserve for other uses)
(global-unset-key "\C-\\")

;; C-DEL WIPWIPWIP
;; TODO all the backspace/delete bindings are messed up
;;(define-key input-decode-map "\377" [(control delete)])

;;;; Control remappings ;;;;

;; Global bindings
(dolist (global-key-rebinding
         `((,(kbd "C-<right>") . forward-sentence)
           (,(kbd "C-<left>") . backward-sentence)
           (,(kbd "M-<up>") . beginning-of-buffer)
           (,(kbd "M-<down>") . end-of-buffer)
           (,(kbd "M-{") . switch-to-prev-buffer)
           (,(kbd "M-}") . switch-to-next-buffer)
           ;;(,(kbd "C-<delete>") . backward-kill-sentence)
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
       ;;(define-key ,(cdr lisp-mode-pair) (kbd "C-<delete>") 'backward-kill-sexp)
       )))

(define-key paredit-mode-map (kbd "RET") 'newline-and-indent)
(define-key paredit-mode-map (kbd "C-\\") 'paredit-convolute-sexp)
(define-key paredit-mode-map (kbd "C-] [") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-] ]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] {") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] }") 'paredit-backward-barf-sexp)
