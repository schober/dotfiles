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

;; Keypad delete, which the Kinesis Advantage has (oddly)
(global-set-key [kp-delete] 'delete-char)

;; Workaround for M-[ (which is used as the xterm CSI) - we escape with C-]
(define-key input-decode-map (kbd "C-] M-[") (kbd "M-["))

;; Globally unbind keys that we don't want modes to grab
(dolist (key '("C-s" "C-d" "C-g" "C-o"
               "M-{" "M-}"))
  (global-unset-key (read-kbd-macro key)))

;; Bind "C-<key>" for keys which we work around with "C-] <key>"
(dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
               "!"     "#" "$" "%" "^" "&" "*" "(" ")"
               "DEL" "RET"))
  (let ((from-key (concat "C-] " key))
        (to-key (concat "C-" key))
        (meta-from-key (concat "C-M-] " key))
        (meta-to-key (concat "C-M-" key)))
    (define-key input-decode-map (read-kbd-macro from-key) (read-kbd-macro to-key))
    (define-key input-decode-map (read-kbd-macro meta-from-key) (read-kbd-macro meta-to-key))))


;;;; Control remappings ;;;;

;; Global bindings
(dolist (global-key-rebinding
         `(("C-s"       save-buffer)
           ("C-d"       keyboard-quit)
           ("C-f"       isearch-forward)
           ("C-g"       goto-line)
           ("C-o"       find-file)
           ("M-{"       switch-to-prev-buffer)
           ("C-M-["     switch-to-prev-buffer)
           ("M-}"       switch-to-next-buffer)
           ("C-M-]"     switch-to-next-buffer)
           ("M-|"       other-window)
           ("C-M-\\"    other-window)
           ("M-`"       other-window)
           ("<home>"    move-beginning-of-line)
           ("<end>"     move-end-of-line)
           ("C-RET"     open-line)
           ("C-DEL"     backward-kill-word)
           ("M-DEL"     backward-kill-sentence)
           ("C-<left>"  backward-word)
           ("C-<right>" forward-word)
           ("M-<left>"  backward-sentence)
           ("M-<right>" forward-sentence)
           ("M-<up>"    beginning-of-buffer)
           ("M-<down>"  end-of-buffer)
           ("ESC ESC"   delete-other-windows)))
  (global-set-key (read-kbd-macro (car global-key-rebinding))
                  (cadr global-key-rebinding)))

;; Minibuffer bindings
(define-key minibuffer-local-map (kbd "C-d") 'abort-recursive-edit)

;; Isearch mode bindings
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)

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
       (define-key ,(cdr lisp-mode-pair) (kbd "M-DEL") 'backward-kill-sexp)
       (define-key ,(cdr lisp-mode-pair) (kbd "C-d") 'keyboard-quit))))

;; Paredit mode bindings
(define-key paredit-mode-map (kbd "RET") 'paredit-newline)
(define-key paredit-mode-map (kbd "C-DEL") 'paredit-backward-kill-word)
(define-key paredit-mode-map (kbd "C-\\") 'paredit-convolute-sexp)
(define-key paredit-mode-map (kbd "C-] [") 'paredit-forward-barf-sexp)
(define-key paredit-mode-map (kbd "C-] ]") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] {") 'paredit-backward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-] }") 'paredit-backward-barf-sexp)


;;;; Aquamacs ;;;;

(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
