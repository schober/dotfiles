#! /bin/emacs
(require 'packages)

(provide 'custom-keyboard)

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
(global-unset-key (kbd "C-]"))

;; C-\ (which I want to reserve for other uses)
(global-unset-key (kbd "C-\\"))

;; Workaround for M-[ (which is used as the xterm CSI) - we escape with C-]
(define-key input-decode-map (kbd "C-] M-[") (kbd "M-["))

;; Keys that we don't want modes to grab from us
(global-unset-key (kbd "C-g"))
(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "M-{"))
(global-unset-key (kbd "M-}"))
(set-quit-char ?\d)

;; Bind "C-<key>" for keys which we work around with "C-] <key>"
(dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
               "!"     "#" "$" "%" "^" "&" "*" "(" ")"
               "DEL"))
  (let ((from-key (concat "C-] " key))
        (to-key (concat "C-" key)))
    (define-key input-decode-map (read-kbd-macro from-key) (read-kbd-macro to-key))))


;;;; Control remappings ;;;;

;; Global bindings
(dolist (global-key-rebinding
         `((,(kbd "C-<left>") . backward-word)
           (,(kbd "C-<right>") . forward-word)
           (,(kbd "M-<left>") . backward-sentence)
           (,(kbd "M-<right>") . forward-sentence)
           (,(kbd "M-<up>") . beginning-of-buffer)
           (,(kbd "M-<down>") . end-of-buffer)
           (,(kbd "M-{") . switch-to-prev-buffer)
           (,(kbd "M-}") . switch-to-next-buffer)
           (,(kbd "C-DEL") . backward-kill-sentence)
           (,(kbd "C-g") . goto-line)
           (,(kbd "C-d") . abort-recursive-edit)
           (,(kbd "C-f") . isearch-forward)))
  (global-set-key (car global-key-rebinding)
                  (cdr global-key-rebinding)))

;; Lisp mode bindings
(dolist (lisp-mode-pair
	 '((emacs . emacs-lisp-mode-map)
	   (emacs . lisp-mode-map)
	   (paredit . paredit-mode-map)))
  (eval-after-load (car lisp-mode-pair)
    `(progn
       (define-key ,(cdr lisp-mode-pair) (kbd "C-<left>") 'backward-word)
       (define-key ,(cdr lisp-mode-pair) (kbd "C-<right>") 'forward-word)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-<left>") 'backward-sexp)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-<right>") 'forward-sexp)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-<up>") 'beginning-of-buffer)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-<down>") 'end-of-buffer)
       (define-key ,(cdr lisp-mode-pair) (kbd "C-DEL") 'backward-kill-sexp)
       (define-key ,(cdr lisp-mode-pair) (kbd "M-d") 'kill-sexp))))

;; Paredit mode bindings
(define-key paredit-mode-map (kbd "RET") 'paredit-newline)
(define-key paredit-mode-map (kbd "C-\\") 'paredit-convolute-sexp)
(define-key paredit-mode-map (kbd "C-] [") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-] ]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] {") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] }") 'paredit-backward-barf-sexp)

;;;; Global Init ;;;;;

;; (cua-mode)

;;;; Aquamacs ;;;;

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
