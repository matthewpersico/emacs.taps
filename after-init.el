;;; after-init.el --- Summary

;; My stuff, post Exordium

;;; Commentary:

;;; ********************
;;; Notes
;;ADAM FAANES
;;12:05:17 What's the Emacs command to put a buffer into merge mode that already has all the <<<<<<< HEAD and >>>>>> stuff in it after a conflict?
;;12:05:39 or is there one? I seem to remember there is one...
;;BART SPIERS
;;12:05:42 M-x smerge-mode
;;ADAM FAANES
;;12:05:54 brilliant  Thanks

;;; Code:

(message "Start after-init.el")

;;; ********************
;;; OS Setup
(defvar mop-myHome)
(cond
 ((string-equal system-type "windows-nt")
  (progn
    (setq mop-myHome (concat (getenv "HOMEDRIVE") (getenv "HOMEPATH")))))
 ((string-equal system-type "gnu/linux")
  (progn
    (setq mop-myHome (getenv "HOME"))))
 ((string-equal system-type "darwin")
  (progn
    (setq mop-myHome (getenv "HOME"))
    (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize)
      (exec-path-from-shell-copy-env "HOSTNAME")
      )
  ))
 )

;;; ********************
;;; Add the taps directory as part of the load path so that I can use
;;; extensions I like without interfering with Exordium
(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/taps/" (getenv "USER") "/lisp"))

;;; And since this should be the last load-path, I can set this now, since
;;; flycheck does not use load-path directly.  From https://stackoverflow.com/questions/20498554/how-do-i-make-flycheck-find-required-file-in-emacs-lisp
(setq-default flycheck-emacs-lisp-load-path 'inherit)

;;; ********************
;;; Key defs

;; Shift backspace is pressed more by accident than anything
;; else. Unassiginging it here seems to change it to delete-to-the-right. I can
;; live with that.
(global-set-key (kbd "S-<delete>") nil)

;; Ctrl PageDown is pressed more by accident than anything
;; else.
(global-set-key (kbd "C-<next>") nil)

;; Turn off Swoop
(global-set-key (kbd "C-S-s") nil) ;; defaults down to C-s

;; Turn off annoying keystroke
(global-set-key (kbd "C-S-a") nil)

;; Function keys
(global-set-key [f1]  'vterm)
(global-set-key [f8]  'start-kbd-macro)
(global-set-key [f9]  'end-kbd-macro)
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key (kbd "<f2> n") 'bury-buffer) ;; buffer rotation, different from
                                             ;; window rotation. Re-mapped from
                                             ;; f11 because the mac and many VM
                                             ;; apps gobble f11 for their own
                                             ;; uses.

;; `narrow-to-region' ("C-x n n") is disabled; most likely to get invoked with
;; those keystrokes when least expected and I never use it.
;;(put 'narrow-to-region 'disabled nil)
;;(global-set-key "\C-XN" 'narrow-to-region) ;rebind to cap
;;(global-set-key "\C-xW" 'widen)         ;rebind to cap

;; next/previous window.
(global-set-key "\C-Xn" 'other-window)
(global-set-key "\C-Xp"
                #'(lambda (arg)
                   "Select the ARG'th previous different window"
                   (interactive "p")
                   (other-window (- arg))))

;; Default function for these is unwanted and keeps asking if I want to enable
;; it. Also useful in markdown files where \C-xn/p are assigned to markdown
;; functions.
(global-set-key "\C-X\C-n" 'other-window)
(global-set-key "\C-X\C-p"
                #'(lambda (arg)
                   "Select the ARG'th previous different window"
                   (interactive "p")
                   (other-window (- arg))))

;; Searching
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-\C-R" 'query-replace-regexp)

;; overrides 'mail'
(global-set-key "\C-xm" 'manual-entry)
(global-set-key "\C-xM" 'manual-entry)

;; Never bound
(global-set-key "\C-xc" 'compile)

;; Echo the current line number
(global-set-key "\C-xw" 'what-line)

;; g is line, G is column
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-G" 'goto-column)

;; Confused with \C-xk when killing buffers
(global-set-key "\C-x\C-k\r" 'ido-kill-buffer)

;; Word completions
(ac-set-trigger-key "TAB")

;; VC and diff
(define-key vc-menu-map [vc-ediff]
  '(menu-item "Ediff with Base Version" vc-ediff
              :help "Ediff file set with the base version"))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

(fringe-helper-define 'git-gutter-fr:added nil
  "...XX..."
  "...XX..."
  "...XX..."
  "XXXXXXXX"
  "XXXXXXXX"
  "...XX..."
  "...XX..."
  "...XX...")
(set-face-foreground 'git-gutter-fr:added    "green")

(fringe-helper-define 'git-gutter-fr:modified nil
  "........"
  "........"
  "..XX...."
  ".XXXX..X"
  "X..XXXX."
  "....XX.."
  "........"
  "........")
(set-face-foreground 'git-gutter-fr:modified   "yellow")

(fringe-helper-define 'git-gutter-fr:deleted nil
  "........"
  "........"
  "........"
  "XXXXXXXX"
  "XXXXXXXX"
  "........"
  "........"
  "........")
(set-face-foreground 'git-gutter-fr:deleted    "red")
;;; this line added
;;; ********************
;;; aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'perl-mode 'cperl-mode) ; always use cperl-mode

;;; ********************
;;; server start
(server-start)

;;; ********************
;;; imenu
(if (featurep 'xemacs)
     (global-set-key [(button3)] 'imenu) ; XEmacs
   (global-set-key [mouse-3] 'imenu)) ; else GNU Emacs
(add-hook 'cperl-mode-hook 'imenu-add-menubar-index)
(add-hook 'perl-mode-hook 'imenu-add-menubar-index)
(add-hook 'sh-mode-hook 'imenu-add-menubar-index)
(which-function-mode 1)

;;; ********************
;;; Display

;; Window and iconified title
(setq frame-title-format (list "emacs@"
                               " - (%f)")
          )

;; gtk-> xaw shenanigans for emacs 27.2
(set-face-background 'scroll-bar "grey")

;;; https://bbgithub.dev.bloomberg.com/bspiers5/emacs-init/blob/master/init.el#L124

(defun toggle-window-split ()
  "Rotate between vertical and horizontal split if we have two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

;;; ********************
;;; Perl
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.tdy\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.deparse\\'" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.ptkdb\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("csperl5.12" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5.16" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5.32" . cperl-mode))
(require 'perltidy)
(defun cperl-mode-hook-for-perltidy ()
  "Keymaps for perltidy functions."
  (local-set-key (kbd "C-c t") 'perltidy-region)
  (local-set-key (kbd "C-c C-t") 'perltidy-buffer))
(add-hook 'cperl-mode-hook 'cperl-mode-hook-for-perltidy)
(require 'perl-find-library)

;;; ********************
;;; Python

;(use-package elpy
;  :ensure t
;  :defer t
;  :init
;  (advice-add 'python-mode :before 'elpy-enable))
(use-package virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

;;; **********************************
;;; Additional file types for spelling
(defun turn-on-flyspell () "Turn on flyspell." (flyspell-mode 1))
(add-hook 'markdown-mode-hook 'turn-on-flyspell)
(add-hook 'change-log-mode-hook 'turn-on-flyspell)

;;; ********************
;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key [f7] 'list-flycheck-errors)

;; The following is thanks to https://github.com/flycheck/flycheck/issues/1436
(defun shellcheck-disable-error-at-point (&optional pos)
  "Insert a shellcheck disable directive at the current error (POS) in the code."
  (interactive)
  (-when-let* ((error (tabulated-list-get-id pos))
               (buffer (flycheck-error-buffer error))
               (id (flycheck-error-id error))
               ;;(message (flycheck-error-message error))
               )
    (when (buffer-live-p buffer)
      (if (eq (window-buffer) (get-buffer flycheck-error-list-buffer))
          ;; When called from within the error list, keep the error list,
          ;; otherwise replace the current buffer.
          (pop-to-buffer buffer 'other-window)
        (switch-to-buffer buffer))
      (let ((pos (flycheck-error-pos error)))
        (unless (eq (goto-char pos) (point))
          ;; If widening gets in the way of moving to the right place, remove it
          ;; and try again
          (widen)
          (goto-char pos))

        ;; Move to start of line with error position.
        (beginning-of-line-text)

        ;; The only error I know of where the disable directive is AFTER the
        ;; error position, not before.
        (when (string-equal id "SC2148")
              (forward-line)
              )
        ;; Insert the disable line
        (insert (format "# shellcheck disable=%s #https://github.com/koalaman/shellcheck/wiki/%s\n" id id))
        ;; Indent it
        (indent-for-tab-command))

      ;; Re-highlight the errors
      (flycheck-error-list-highlight-errors 'preserve-pos))))

(define-key flycheck-error-list-mode-map (kbd "d") #'shellcheck-disable-error-at-point)

;;
;; Refine to remove the --shell option, from flycheck.el
;;
;; doesn't work
;;(setf (flycheck-checker-get 'sh-shellcheck 'command)
;;      (remove "-shell" (flycheck-checker-get 'sh-shellcheck 'command)))

(flycheck-define-checker sh-shellcheck
  "A shell script syntax and style checker using Shellcheck.

See URL `https://github.com/koalaman/shellcheck/'."
  :command ("shellcheck"
            "--format" "checkstyle"
            ;; "--shell" (eval (symbol-name sh-shell))
            (option-flag "--external-sources" flycheck-shellcheck-follow-sources)
            (option "--exclude" flycheck-shellcheck-excluded-warnings list
                    flycheck-option-comma-separated-list)
            "-")
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter
  (lambda (errors)
    (flycheck-remove-error-file-names
     "-" (flycheck-dequalify-error-ids errors)))
  :modes sh-mode
  :predicate (lambda () (memq sh-shell flycheck-shellcheck-supported-shells))
  :verify (lambda (_)
            (let ((supports-shell (memq sh-shell
                                        flycheck-shellcheck-supported-shells)))
              (list
               (flycheck-verification-result-new
                :label (format "Shell %s supported" sh-shell)
                :message (if supports-shell "yes" "no")
                :face (if supports-shell 'success '(bold warning)))))))

;;; ********************
;;; Whitespace - Turn off delete trail on save so that PRs and quilts don't get
;;; extraneous changes. ws-butler will remove trailing whitespace as you go,
;;; only on lines you edit.
(use-package ws-butler)
(ws-butler-global-mode)
(global-delete-trailing-whitespace-mode -1)

;;; ********************
;;; Exordium overrides

;;; None

;;; ********************
;;; File sets
(filesets-init)

;;; ********************
;;; Popper
;;; https://www.youtube.com/watch?v=E-xUNlZi3rI

;;; ********************
;;; Locally created functions

;;
;; Quickie convenience functions and their key mappings
;;

;;; https://bbgithub.dev.bloomberg.com/bspiers5/emacs-init/blob/master/init.el#L188
(defadvice kill-line (before kill-line-autoreindent activate)
  "Kill excess whitespace when joining lines.
If the next line is joined to the current line, kill the extra indent whitespace in front of the next line."
  (when (and (eolp) (not (bolp)))
    (save-excursion
      (forward-char 1)
      (just-one-space 1))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))
(global-set-key (kbd "C-x K") 'kill-other-buffers)

(defun MOP-insert-buffer-name ()
  "MOP - Insert the buffer name into the buffer at the current editing point."
  (interactive "*")
  (insert (buffer-name)))
(global-set-key (kbd "<f12>") 'MOP-insert-buffer-name)

(defun MOP-insert-function-name ()
  "MOP - Insert the function name into the buffer at the current editing point."
  (interactive "*")
  (insert (add-log-current-defun)))
(global-set-key (kbd "S-<f12>") 'MOP-insert-function-name)

(defun MOP-insert-buffer-list ()
  "MOP - PLACEHOLDER, DOES NOT WORK YET! Insert the list of all buffer names into the buffer at the current editing point."
  (interactive "*")
  (buffer-list t))

(defun MOP-insert-now-time ()
  "MOP - Insert the current time into the buffer at the current editing point."
  (interactive "*")
  (insert (current-time-string)))

(global-set-key "\C-x\C-z" 'delete-trailing-whitespace)

(defun MOP-strip-start-whitespace ()
  "MOP - All leading whitespace gets chucked."
  (interactive "*")
  (save-excursion
    (goto-char(point-min)) ;; Faster than beginning-of-buffer
    (while (re-search-forward "^[ \t]+" nil t)
      (replace-match "" nil nil)))
  (message "MOP-strip-start-whitespace complete."))
(global-set-key "\C-xZ" 'MOP-strip-start-whitespace)

(defun MOP-upcase-region-or-char (arg)
  "If a region is selected, uppercase it.  Else, if cursor is on a -, replace it with _.  Else uppercase the current char (ARG)."
  (interactive "p")
  (cond ((region-active-p)
         (upcase-region (region-beginning) (region-end)))
        ((looking-at "-")
         (delete-char 1 nil)
         (insert "_"))
        (t
         (let ((beg (point)))
           (forward-char arg)
           (upcase-region beg (point))))))
(global-set-key "\C-x\C-u" 'MOP-upcase-region-or-char)

(defun MOP-downcase-region-or-char (arg)
  "If a region is selected, lowercase it.  Else, if cursor is on a -, replace it with _.  Else lowercase the current char (ARG)."
  (interactive "p")
  (cond ((region-active-p)
         (downcase-region (region-beginning) (region-end)))
        ((looking-at "_")
         (delete-char 1 nil)
         (insert "-"))
        (t
         (let ((beg (point)))
           (forward-char arg)
           (downcase-region beg (point))))))
(global-set-key "\C-x\C-l" 'MOP-downcase-region-or-char)

;; Popup buffer list on shift button 3. - FAQ
(defun MOP-popup-buffer-list ()
  "Popup buffer menu."
  (interactive "@")
  (run-hooks 'activate-menubar-hook)
  (popup-menu (car (find-menu-item current-menubar '("Buffers")))))
(define-key global-map [(shift button3)] 'MOP-popup-buffer-list)

(defun MOP-standard-make ()
  "MOP - set the normal compile command and kick it off."
  (interactive "i")
  (setq compile-command "make ")
  (call-interactively 'compile))
;; NOT BOUND

;; Whipping an alien C-source file into shape.  Do bunches of
;; query-replaces, because most of these things benefit from the
;; attention of a human eyeball --- the regexps aren't perfect
(defun MOP-fix-C-formatting ()
  "MOP - Whipping an alien C-source file into shape."
  (interactive)

  ; Convert all tab characters into appropriate sequences of space
  ; chars (makes diffs indent properly, and reduces complexity of
  ; expressions to follow).
  (untabify (point-min) (point-max))

  ; Nail trailing whitespace
  (MOP-strip-end-whitespace)

  ; Puh-leeze, death to those leading commas in lists!  Make them trail correctly.
  (goto-char (point-min))
  (query-replace-regexp "\\(\n +\\)," ",\\1" nil)

  ; We want a space between keywords and parens.  We'd like to force
  ; *no* space between user functions and parens, as well, but then
  ; we'd end up having to either write a "not-these" regexp (probably
  ; possible; will look later), or make two passes over the keywords
  ; (annoying for user if interactive).
  (goto-char (point-min))
  (query-replace-regexp "\\(if\\|for\\|while\\|switch\\|return\\)(" "\\1 (" nil)

  ; Keywords opening blocks (if, while, etc...) get their opening
  ; braces on the same line, not the next.
  (goto-char (point-min))
  (query-replace-regexp ")\n +{" ") {" nil)

  ; special case to avoid extra passes: cuddle
  ; closing-curly-else-opening-curly sets on the same line, not three
  (goto-char (point-min))
  (query-replace-regexp "}\n +else\n +{" "} else {" nil)

  ; Any elses not in that configuration also follow closing curly on
  ; the same line, not the next
  (goto-char (point-min))
  (query-replace-regexp "}\n +else" "} else" nil)

  ; if following an else: same line (but user may need to have a look at this)
  (goto-char (point-min))
  (query-replace-regexp "else\n +if " "else if " nil)

  ; We like a single space after our intraline commas (user needs to
  ; ensure we're not in a string; we're not trying to be that
  ; sophisticated in the regexp)
  (goto-char (point-min))
  (query-replace-regexp ",\\([^ \n]\\)" ", \\1" nil)

  ; A potentially time-consuming one...  re-indent major expressions
  ; (including functions)
  (goto-char (point-min))
  (while (re-search-forward "^\\( *typedef[^\n]*\\)*{" nil t)
    (backward-char)
    (redisplay-frame)                   ;So the poor bored user can see what we're up to
    (c-indent-exp)
    (forward-char)
    (redisplay-frame) (sleep-for 1))    ;...and what may have happened

  (message "Mechanical reformat done.  Now retouch by eye.")
  )

(defun MOP-regexp-replace-across-buffers ()
  "MOP - Meta-x ibuffer RET t Q."
  (interactive "*")
  (message "Use the command sequence M-x ibuffer RET t Q"))

;;; ********************
;;; My saved keyboard macros
;;; 1) Record a macro.
;;; 2) Name it (C-x C-k n). If you name all keyboard macros starting with kbm-,
;;;    then you can get a list of them using tab completion.
;;; 3) Edit the macros file (C-x f ~/.emacs.d/taps/mpersico5/lisp/kbmacros.el).
;;; 4) Write the macro to the file (M-x insert-kbd-macro).
;;; 5) Save.
;;; 6) In order to run the macro, invoke like any other function
;;;    (M-x name)
(require 'kbmacros)

;;; ********************
;;; Overrides

;;; Not sure what this is, but here it is anyway, moved from init.el.
(put 'erase-buffer 'disabled nil)

;;; imenu does not recognize foo-bar() as a function. We fix that here.
(require 'sh-script-imode)

;;; Make found search locations vertically center
;(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

(defadvice
    isearch-forward
    (after isearch-forward-recenter activate)
  "String to shut up flycheck." (recenter))
(ad-activate 'isearch-forward)

(defadvice
    isearch-backward
    (after isearch-backward-recenter activate)
  "String to shut up flycheck." (recenter))
(ad-activate 'isearch-backward)

(defadvice
    isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
  "String to shut up flycheck." (recenter))
(ad-activate 'isearch-repeat-forward)

(defadvice
    isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
  "String to shut up flycheck." (recenter))
(ad-activate 'isearch-repeat-backward)

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
;; This turns off local lock files to avoid rsync issues, but my git commit hook
;;  checks them. So we put them back and we fix the rsync.
;; '(create-lockfiles nil)

;; This was removed from custom-set-variables because it does not seem to play
;; with Perl 5.16:
;; '(flycheck-perl-include-path (quote ("lib/perl" "../lib/perl" "")))

(message "End after-init.el")
(provide `after-init)
;;; after-init ends here
