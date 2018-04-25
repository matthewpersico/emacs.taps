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
(add-to-list 'load-path (concat "~/.emacs.d/taps/" (getenv "USER") "/lisp"))

;;; ********************
;;; Key defs

;; Shift backspace is pressed more by accident than anything
;; else. Unassiginging it here seems to change it to delete-to-the-right. I can
;; live with that.
(global-set-key (kbd "S-<delete>") nil)

;; Turn off Swoop
(global-set-key (kbd "C-S-s") nil) ;; defaults down to C-s

;; Turn off annoying keystroke
(global-set-key (kbd "C-S-a") nil)

;; Function keys
(global-set-key [f1]  'shell)
(global-set-key [f8]  'start-kbd-macro)
(global-set-key [f9]  'end-kbd-macro)
(global-set-key [f10] 'call-last-kbd-macro)
(global-set-key [f11] 'bury-buffer) ;; buffer rotation, different from window
                                    ;; rotation
(global-set-key (kbd "<f2> n") 'bury-buffer) ;; buffer rotation, different from
                                             ;; window rotation. Mapped for
                                             ;; when f11 is gobbled up by the
                                             ;; browser of a vpn session

;; `narrow-to-region' ("C-x n n") is disabled; most likely to get invoked with
;; those keystrokes when least expected and I never use it.
;;(put 'narrow-to-region 'disabled nil)
;;(global-set-key "\C-XN" 'narrow-to-region) ;rebind to cap
;;(global-set-key "\C-xW" 'widen)         ;rebind to cap

;; next/previous window
(global-set-key "\C-Xn" 'other-window)
(global-set-key "\C-Xp"
                '(lambda (arg)
                   "Select the ARG'th previous different window"
                   (interactive "p")
                   (other-window (- arg))))
;; Default function for these is unwanted and keeps asking if I want to enable
;; it.
(global-set-key "\C-X\C-n" 'other-window)
(global-set-key "\C-X\C-p"
                '(lambda (arg)
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

;; ediff
(setq ediff-split-window-function 'split-window-horizontally)

;; VC and diff
(define-key vc-menu-map [vc-ediff]
  '(menu-item "Ediff with Base Version" vc-ediff
              :help "Ediff file set with the base version"))

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;;; ********************
;;; aliases
(defalias 'yes-or-no-p 'y-or-n-p) ; y or n is enough
(defalias 'perl-mode 'cperl-mode) ; always use cperl-mode

;;; ********************
;;; recent files
;; EXORDIUM NOT NEEDED: (require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
;; EXORDIUM NOT NEEDED: (recentf-mode 1)

;;; ********************
;;; server start
(server-start)

;;; ********************
;;; imenu
(if (featurep 'xemacs)
     (global-set-key [(button3)] 'imenu) ; XEmacs
   (global-set-key [mouse-3] 'imenu)) ; GNU Emacs
(add-hook 'cperl-mode-hook 'imenu-add-menubar-index)
(add-hook 'perl-mode-hook 'imenu-add-menubar-index)
(add-hook 'sh-mode-hook 'imenu-add-menubar-index)

;;; ********************
;;; Shell
(require 'bash-completion)
(bash-completion-setup)

;;; ********************
;;; Display

;; Window and iconified title
(setq frame-title-format (list "emacs@"
                               (substring (getenv "HOSTNAME")
                                          0
                                          (string-match-p (regexp-quote "\.") (getenv "HOSTNAME"))
                                          )
                               " - (%f)")
      )

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
(require 'perltidy)
(defun cperl-mode-hook-for-perltidy ()
  (local-set-key (kbd "C-c t") 'perltidy-region)
  (local-set-key (kbd "C-c C-t") 'perltidy-buffer))
(add-hook 'cperl-mode-hook 'cperl-mode-hook-for-perltidy)

;;; **********************************
;;; Additional file types for spelling
(defun turn-on-flyspell () (flyspell-mode 1))
(add-hook 'markdown-mode-hook 'turn-on-flyspell)
(add-hook 'change-log-mode-hook 'turn-on-flyspell)

;;; ********************
;;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; The following is thanks to https://github.com/flycheck/flycheck/issues/1436
(defun shellcheck-disable-error-at-point (&optional pos)
  "Insert a shellcheck disable directive at the current error in the code."
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
        (insert (format "# shellcheck disable=%s\n" id))
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
;;; Exordium overrides

;;; None

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
  "MOP - Insert the buffer name into the buffer at the current editing point"
  (interactive "*")
  (insert-string (buffer-name)))
(global-set-key (kbd "<f12>") 'MOP-insert-buffer-name)

(defun MOP-insert-function-name ()
  "MOP - Insert the function name into the buffer at the current editing point"
  (interactive "*")
  (insert-string (add-log-current-defun)))
(global-set-key (kbd "S-<f12>") 'MOP-insert-function-name)

(defun MOP-insert-buffer-list ()
  "MOP - PLACEHOLDER, DOES NOT WORK YET! Insert the list of all buffer names into the buffer at the current editing point."
  (interactive "*")
  (buffer-list t))

(defun MOP-insert-now-time ()
  "MOP - Insert the current time into the buffer at the current editing point"
  (interactive "*")
  (insert-string (current-time-string)))

(defun MOP-strip-end-whitespace ()
  "MOP - All trailing whitespace gets chucked"
  (interactive "*")
  (save-excursion
    (goto-char(point-min)) ;; Faster than beginning-of-buffer
    (while (re-search-forward "[ \t]+$" nil t)
      (replace-match "" nil nil)))
  (message "MOP-strip-end-whitespace complete."))
(global-set-key "\C-x\C-z" 'delete-trailing-whitespace) ;MOP-strip-end-whitespace)

(defun MOP-strip-start-whitespace ()
  "MOP - All leading whitespace gets chucked"
  (interactive "*")
  (save-excursion
    (goto-char(point-min)) ;; Faster than beginning-of-buffer
    (while (re-search-forward "^[ \t]+" nil t)
      (replace-match "" nil nil)))
  (message "MOP-strip-start-whitespace complete."))
(global-set-key "\C-xZ" 'MOP-strip-start-whitespace)

(defun MOP-upcase-region-or-char (arg)
  "LOCAL: Uppercase the region, else uppercase the char under the cursor."
  (interactive "p")
  (if (region-active-p)
      (upcase-region (region-beginning) (region-end))
    ( progn (let ((beg (point))) (forward-char arg) (upcase-region beg (point))))
   )
  )
(global-set-key "\C-x\C-u" 'MOP-upcase-region-or-char)

(defun MOP-downcase-region-or-char (arg)
  "LOCAL: Lowercase the region, else lowercase the char under the cursor."
  (interactive "p")
  (if (region-active-p)
      (downcase-region (region-beginning) (region-end))
    ( progn (let ((beg (point))) (forward-char arg) (downcase-region beg (point))))
   )
  )
(global-set-key "\C-x\C-l" 'MOP-downcase-region-or-char)

;; Popup buffer list on shift button 3. - FAQ
(defun MOP-popup-buffer-list ()
  "Popup buffer menu."
  (interactive "@")
  (run-hooks 'activate-menubar-hook)
  (popup-menu (car (find-menu-item current-menubar '("Buffers")))))
(define-key global-map [(shift button3)] 'MOP-popup-buffer-list)

(defun MOP-standard-make ()
  "MOP - set the normal compile command and kick it off"
  (interactive "i")
  (setq compile-command "make ")
  (call-interactively 'compile))
;; NOT BOUND

;; Whipping an alien C-source file into shape.  Do bunches of
;; query-replaces, because most of these things benefit from the
;; attention of a human eyeball --- the regexps aren't perfect
(defun MOP-fix-C-formatting ()
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
  "MOP - M-x ibuffer RET t Q"
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

;;; imenu does not recognize foo-bar() as a function. We fix that here:
(require 'sh-script-imode)

;;; Make found search locations vertically center
;(add-hook 'isearch-mode-end-hook 'recenter-top-bottom)

(defadvice
    isearch-forward
    (after isearch-forward-recenter activate)
    (recenter))
(ad-activate 'isearch-forward)

(defadvice
    isearch-backward
    (after isearch-backward-recenter activate)
    (recenter))
(ad-activate 'isearch-backward)

(defadvice
    isearch-repeat-forward
    (after isearch-repeat-forward-recenter activate)
    (recenter))
(ad-activate 'isearch-repeat-forward)

(defadvice
    isearch-repeat-backward
    (after isearch-repeat-backward-recenter activate)
    (recenter))
(ad-activate 'isearch-repeat-backward)

;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
;; This turns off local lock files to avoid rsync issues, but my git commit hook
;;  checks them. So we put them back and we fix the rsync.
;; '(create-lockfiles nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-trigger-key "TAB")
 '(add-log-always-start-new-record t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.emacs.saves"))))
 '(blink-cursor-mode nil)
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(c-basic-offset 4)
 '(change-log-default-name "SourceCtrlLog")
 '(column-number-mode t)
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 2)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(cperl-tab-always-indent t)
 '(custom-enabled-themes (quote (tomorrow-night-bright)))
 '(custom-safe-themes
   (quote
    ("6a18a817e5a1d220a8de8af5d6e5f4619fe3df61dd2cbc37b9acd8d77d42e026" "f5519676e9580060b510012ffde3b41dd5392a3debc98a2b02995499a086a7d4" default)))
 '(exordium-backup-files t)
 '(exordium-enable-y-or-n t)
 '(exordium-highlight-linum t)
 '(explicit-shell-file-name nil)
 '(global-linum-mode t)
 '(global-visual-line-mode nil)
 '(imenu-max-item-length nil)
 '(imenu-sort-function (quote imenu--sort-by-name))
 '(kept-new-versions 6)
 '(kill-whole-line t)
 '(line-move-visual nil)
 '(nyan-animate-nyancat t)
 '(nyan-mode t)
 '(package-selected-packages
   (quote
    (bash-completion flycheck-status-emoji flycheck-pos-tip flycheck-color-mode-line graphviz-dot-mode emojify yasnippet vlf rainbow-delimiters project-explorer powerline paredit page-break-lines ox-gfm org-bullets nlinum modern-cpp-font-lock markdown-mode magit impatient-mode iedit ido-ubiquitous highlight-symbol helm-swoop helm-rtags helm-projectile helm-descbinds helm-ag groovy-mode git-timemachine git-gutter-fringe fill-column-indicator expand-region exec-path-from-shell evil eval-sexp-fu enh-ruby-mode diminish default-text-scale company-rtags cmake-mode cider auto-complete-c-headers all-the-icons ace-window ac-rtags ac-js2)))
 '(protect-buffer-bury-p nil)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(track-eol t)
 '(truncate-lines t)
 '(version-control t)
 '(word-wrap nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 140 ))))
 '(cperl-array-face ((t (:foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))))
 '(cursor ((t (:background "spring green"))))
 '(ediff-even-diff-A ((t (:foreground "#969896" :inverse-video nil))))
 '(ediff-even-diff-B ((t (:foreground "#969896" :inverse-video nil))))
 '(linum-highlight-face ((t (:background "white smoke" :distant-foreground "black")))))
