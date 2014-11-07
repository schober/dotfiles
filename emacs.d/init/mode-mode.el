(provide 'mode-mode)

(require 'cl)

(defun get-available-modes (&optional filter is-negative?)
  (let ((modes))
    ;; Compute available modes
    (mapatoms
     (lambda (fn)
       (when (fboundp fn)
         (let ((func-name (symbol-name fn)))
           (when (string-match "^.*-mode$" func-name)
             (push (cons (substring func-name 0 -5)
                         fn)
                   modes))))))
    
    ;; Apply filtering (if any)
    (if filter
        (if is-negative?
            (set-difference modes filter :test (lambda (x y) (eq (cdr x) y)))
          
          ;; cl-intersection implemented with two set differences because intersection is unstable
          (set-difference modes
                          (set-difference modes filter :test (lambda (x y) (eq (cdr x) y)))))
      modes)))

(defun prompt-for-mode (prompt &optional filter is-negative?)
  (let ((modes (get-available-modes filter is-negative?)))
    (let ((selected-mode-name (completing-read prompt modes nil t)))
      (cdr (assoc selected-mode-name modes)))))

(defun mode-toggle ()
  (interactive)
  (funcall (prompt-for-mode "Toggle Mode: ")
           'toggle))

(defun mode-enable ()
  (interactive)
  (let* ((enabled-minor-modes (remove-if-not (lambda (sym)
                                               (and (boundp sym)
                                                    (symbol-value sym)))
                                             minor-mode-list))
         (enabled-modes (cons major-mode enabled-minor-modes))
         (selected-mode (prompt-for-mode "Enable Mode: " enabled-modes t)))
    (funcall selected-mode)))

(defun mode-disable ()
  (interactive)
  (funcall (prompt-for-mode "Disable Mode: " minor-mode-list nil)
           -1))
