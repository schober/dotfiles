#! /bin/emacs

(provide 'keyboard)

;;;; Keyboard input remappings ;;;;

;; M-<arrow>
(define-key input-decode-map "\e[1;4A" [(meta up)])
(define-key input-decode-map "\e[1;4B" [(meta down)])
(define-key input-decode-map "\e[1;4C" [(meta right)])
(define-key input-decode-map "\e[1;4D" [(meta left)])

;; S-M-<arrow>
(define-key input-decode-map "\e[1;10A" [(shift meta up)])
(define-key input-decode-map "\e[1;10B" [(shift meta down)])
(define-key input-decode-map "\e[1;10C" [(shift meta right)])
(define-key input-decode-map "\e[1;10D" [(shift meta left)])

;; C-M-<arrow>
(define-key input-decode-map "\e[1;8A" [(control meta up)])
(define-key input-decode-map "\e[1;8B" [(control meta down)])
(define-key input-decode-map "\e[1;8C" [(control meta right)])
(define-key input-decode-map "\e[1;8D" [(control meta left)])

;;;; Control remappings ;;;;

(global-set-key (kbd "C-<right>") 'forward-sentence)
(global-set-key (kbd "C-<left>") 'backward-sentence)
(define-key emacs-lisp-mode-map (kbd "C-<right>") 'forward-sexp)
(define-key emacs-lisp-mode-map (kbd "C-<left>") 'backward-sexp)
