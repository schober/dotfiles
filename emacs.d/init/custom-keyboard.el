#! /bin/emacs
(require 'packages)

(provide 'custom-keyboard)

;; TODO: Add support for key bindings attached to a hook.
;; TODO: Bind all global keys in a minor mode and keep that mode on top of the minor-mode-alist.
;;       http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/1758639

;;;; INPUT remappings ;;;;


(case (window-system)

  ;; Proper windowed systems.
  (('x 'ns 'w32)
   ;; TODO: Some day, fix all the keys...   
  )
  
  ;; Terminal systems. What a modern world we live in!
  (t
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

   ;; C-\ (which I want to reserve for other uses)
   (global-unset-key (kbd "C-\\"))

   ;; We use C-] as an escape character for a "control" sequence (like meta)
   (global-unset-key (kbd "C-]"))

   ;; Workaround for M-[ (which is used as the xterm CSI) - we escape with C-]
   (define-key input-decode-map (kbd "C-] M-[") (kbd "M-["))

   ;; Bind "C-<key>" and "C-M-<key>" for keys which we work around with "C-] <key>" "and "C-M-] <key>"
   (dolist (key '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                  "!" "@" "#" "$" "%" "^" "&" "*" "(" ")"
                  "`" "~" "=" "+" ";" ":" "," "." "<" ">"
                  "'" "?" "|" "\""
                  "TAB" "SPC" "RET" "DEL"))
     (let ((from-key (concat "C-] " key))
           (to-key (concat "C-" key))
           (meta-from-key (concat "C-M-] " key))
           (meta-to-key (concat "C-M-" key)))
       (define-key input-decode-map (read-kbd-macro from-key) (read-kbd-macro to-key))
       (define-key input-decode-map (read-kbd-macro meta-from-key) (read-kbd-macro meta-to-key))))))


;;;; CONTROL remappings ;;;;

  ;; Globally unbind keys that we don't want modes to grab
(dolist (globally-unbound-key
         '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
           "o" "p" "s" "d" "v" "b" "n"
           ;; Beware the Untouchables: I (TAB), @ (SPC), M (RET), ? (DEL)
           ))
  (dolist (prefix '("C-" "M-" "C-M-"))
    (global-unset-key (read-kbd-macro (concat prefix globally-unbound-key)))))

(dolist (key '("C-o"  "C-@"
               "M-l" "M-{" "M-}"
               "C-M-[" "C-M-]"
               "M-ESC" "ESC ESC ESC"))
  (global-unset-key (read-kbd-macro key)))

;; A meaty convenience
(defmacro my-bind-keys (&rest modes-bindings)
  (flet ((postprocess-key (key)
           (declare (string key))
           (case (window-system)
             ((pc nil)
              key)
             (t
              (replace-regexp-in-string
               ;; XEmacs et al can correctly interpret, e.g., "C-]". Replace "C-] ]" with "C-]".
               "-\\][ \t]+" "-"
               key)))))
   `(progn
      ,@(map 'list
             (lambda (mode-bindings)
               (let* ((modes      (car mode-bindings))
                      (modes-list (if (listp modes)
                                      (if (listp (car modes))
                                          (cdr modes)
                                        modes)
                                    (list modes)))
                      (hook (when (and (listp modes)
                                     (listp (car modes)))
                              (caar modes)))
                      (bindings   (cdr mode-bindings)))
                 `(progn
                    ,@(map 'list
                           (lambda (mode)
                             `(progn
                                ,@(map 'list
                                       (lambda (binding)
                                         (let ((key (postprocess-key (car binding)))
                                               (fn  (cadr binding)))
                                           (if (eq 'global mode)
                                               `(global-set-key (read-kbd-macro ,key) ',fn)
                                             `(define-key ,mode (read-kbd-macro ,key) ',fn))))
                                       bindings)))
                           modes-list))))
             modes-bindings))))

(my-bind-keys
 (global
  ("C-s"       save-buffer)
  ("M-s"       set-visited-file-name)
  ("C-d"       keyboard-quit)
  ("C-f"       isearch-forward)
  ("M-f"       replace-regexp)
  ("C-M-f"     isearch-forward-regexp)
  ("C-g"       goto-line)
  ("C-k"       kill-whole-line)
  ("C-o"       find-file)
  ("M-o"       find-file-read-only)
  ("C-p i"     package-install)
  ("C-p l"     package-list-packages)
  ("C-v"       quoted-insert)
  ("C-b"       switch-to-buffer)
  ("M-b"       ibuffer)
  ("C-n"       make-frame)
  ("M-m"       mode-enable)
  ("M-M"       mode-disable)
  ("C-M-m"     mode-toggle)
  ("C-,"       customize)
  ("C-1"       delete-other-windows)
  ("C-2"       split-window-below)
  ("C-3"       split-window-right)
  ("C-6"       kill-this-buffer)
  ("M-6"       delete-frame)
  ("C-0"       delete-window)
  ("M-{"       switch-to-prev-buffer)
  ("C-M-] ["   switch-to-prev-buffer)
  ("M-}"       switch-to-next-buffer)
  ("C-M-] ]"   switch-to-next-buffer)
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
  ("M-<down>"  end-of-buffer))
 
 (minibuffer-local-map
  ("C-d" abort-recursive-edit))

 (isearch-mode-map
  ("C-f" isearch-repeat-forward)
  ("C-d" isearch-abort))

 ((emacs-lisp-mode-map lisp-mode-map) 
  ("M-f" replace-regexp)
  ("M-k" kill-sexp))
 
 ((emacs-lisp-mode-map lisp-mode-map paredit-mode-map)
  ("C-<left>"      backward-word)
  ("C-<right>"     forward-word)
  ("M-<left>"      backward-sexp)
  ("M-<right>"     forward-sexp)
  ("M-<up>"        beginning-of-buffer)
  ("M-<down>"      end-of-buffer)
  ;; ("M-DEL"         backward-kill-sexp)
  ("M-<backspace>" backward-kill-sexp)
  ("C-d"           keyboard-quit))

 (paredit-mode-map
  ("RET"           paredit-newline)
  ("C-DEL"         paredit-backward-kill-word)
  ("C-<backspace>" paredit-backward-kill-word)
  ("C-\\"          paredit-convolute-sexp)
  ;("C-] ["         paredit-forward-barf-sexp)
  ;("C-] ]"         paredit-forward-slurp-sexp)
  ;("C-] {"         paredit-backward-slurp-sexp)
  ;("C-] }"         paredit-backward-barf-sexp)
  ("M-("           paredit-wrap-round)
  ("M-["           paredit-wrap-square)
  ;; ("M-{"           paredit-wrap-curly)
  ("M-K"           paredit-splice-sexp-killing-backward))

; ((compilation-mode-hook) compilation-mode-map
;  ("C-M-] ["   switch-to-prev-buffer)
;  ("C-M-] ]"   switch-to-next-buffer)
 )


;;;; XEmacs ;;;;

(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)
(setq mac-right-command-modifier 'meta)
