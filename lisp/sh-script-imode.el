;;; sh-script-imode.el

;;; imenu does not recognize foo-bar() as a function. We fix that here:

;;; First, make sure to load shell mode.
(require 'sh-script)

;;; Now, redefine the function, ripped out of sh-script.el. We simply added '-'
;;; next to each '_' in the regexps.
(defun sh-current-defun-name ()
  "Advice for sh-current-defun-name
 adding foo-bar as a legal function name."
  (save-excursion
    (end-of-line)
    (when (re-search-backward
           (concat "\\(?:"
                   ;; function FOO
                   ;; function FOO()
                   "^\\s-*function\\s-+\\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?"
                   "\\)\\|\\(?:"
                   ;; FOO()
                   "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()"
                   "\\)\\|\\(?:"
                   ;; FOO=
                   "^\\([[:alpha:]_][[:alnum:]_]*\\)="
                   "\\)")
           nil t)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)
          (match-string-no-properties 3))))
  )

;;; Also, we custom defined sh-imenu-generic-expression by adding the '-' where
;;; needed. See below.
(setq sh-imenu-generic-expression
      (quote
       ((sh
         (nil "^\\s-*function\\s-+\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*\\(?:()\\)?" 1)
         (nil "^\\s-*\\([[:alpha:]_-][[:alnum:]_-]*\\)\\s-*()" 1)))))

(provide 'sh-script-imode)
