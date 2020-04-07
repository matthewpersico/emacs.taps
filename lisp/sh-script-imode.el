;;; sh-script-imode.el --- imenu does not recognize some legal bash constructs.

;;; Commentary:
;;; Dashes in function names.  2016-02-23
;;; Colons in function names.  2020-04-06

;;; Code:

;;; First, make sure to load shell mode.
(require 'sh-script)

;;; Now, redefine the function checker, ripped out of sh-script.el. We simply
;;; added ':-' next to each '_' in the regexps. Well, we first added '-' which
;;; was fine, but then we added ':' which had the adverse effects of:
;;;
;;; 1 - now the dash was not the last char in a range, so it was now a range
;;; definition char, not a char
;;;
;;; 2 - the colon WAS the last char in a range, defining a new range name that
;;; did not exist.
(defun sh-current-defun-name ()
  "Advice for sh-current-defun-name, adding - and : as legal function name chars."
  (save-excursion
    (end-of-line)
    (when (re-search-backward
           (concat "\\(?:"
                   ;; function FOO
                   ;; function FOO()
                   "^\\s-*function\\s-+\\\([[:alpha:]_:-][[:alnum:]_:-]*\\)\\s-*\\(?:()\\)?"
                   "\\)\\|\\(?:"
                   ;; FOO()
                   "^\\s-*\\([[:alpha:]_:-][[:alnum:]_:-]*\\)\\s-*()"
                   "\\)\\|\\(?:"
                   ;; FOO=
                   "^\\([[:alpha:]_:-][[:alnum:]_:-]*\\)="
                   "\\)")
           nil t)
      (or (match-string-no-properties 1)
          (match-string-no-properties 2)
          (match-string-no-properties 3))))
  )

;;; Also, we custom defined sh-imenu-generic-expression by adding the ':-'
;;; where needed. See below.
(setq sh-imenu-generic-expression
      (quote
       ((sh
         (nil "^\\s-*function\\s-+\\([[:alpha:]_:-][[:alnum:]_:-]*\\)\\s-*\\(?:()\\)?" 1)
         (nil "^\\s-*\\([[:alpha:]_:-][[:alnum:]_:-]*\\)\\s-*()" 1)))))

(provide 'sh-script-imode)

;;; Now fixup the sh-script definition of a function:
(defvar sh-font-lock-keywords-var
  '((csh sh-append shell
	 ("\\${?[#?]?\\([[:alpha:]_][[:alnum:]_]*\\|0\\)" 1
          font-lock-variable-name-face))

    (es sh-append executable-font-lock-keywords
	("\\$#?\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\)" 1
         font-lock-variable-name-face))

    (rc sh-append es)
    (bash sh-append sh ("\\$(\\(\\sw+\\)" (1 'sh-quoted-exec t) ))
    (sh sh-append shell
	;; Variable names.
	("\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\)" 2
	  font-lock-variable-name-face)
	;; Function names.
	("^\\(\\sw+\\)[ \t]*(" 1 font-lock-function-name-face)
	("\\<\\(function\\)\\>[ \t]*\\(\\sw+\\)?"
	  (1 font-lock-keyword-face) (2 font-lock-function-name-face nil t))
	("\\(?:^\\s *\\|[[();&|]\\s *\\|\\(?:\\s +-[ao]\\|if\\|else\\|then\\|while\\|do\\)\\s +\\)\\(!\\)"
	 1 font-lock-negation-char-face))

    ;; The next entry is only used for defining the others
    (shell
           ;; Using font-lock-string-face here confuses sh-get-indent-info.
           ("\\(^\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\\\)$" 3 'sh-escaped-newline)
	   ("\\\\[^[:alnum:]]" 0 font-lock-string-face)
	   ("\\${?\\([[:alpha:]_][[:alnum:]_]*\\|[0-9]+\\|[$*_]\\)" 1
	     font-lock-variable-name-face))
    (rpm sh-append rpm2
	 ("%{?\\(\\sw+\\)"  1 font-lock-keyword-face))
    (rpm2 sh-append shell
	  ("^Summary:\\(.*\\)$" (1 font-lock-doc-face t))
	  ("^\\(\\sw+\\):"  1 font-lock-variable-name-face)))
  "Default expressions to highlight in Shell Script modes.  See `sh-feature'.")

(provide 'sh-script)

;;; sh-script-imode.el ends here
