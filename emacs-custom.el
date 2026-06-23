(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start 2 nil nil "Customized with use-package auto-complete")
 '(ac-ignore-case nil nil nil "Customized with use-package auto-complete")
 '(ac-trigger-key "TAB")
 '(add-log-always-start-new-record t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(backup-by-copying t)
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
 '(custom-enabled-themes '(tomorrow-night-bright))
 '(custom-safe-themes
   '("7e50b79d0ba825c619797088f80920282f882a9701f675f58d62cd6f108815ef"
     "6a18a817e5a1d220a8de8af5d6e5f4619fe3df61dd2cbc37b9acd8d77d42e026"
     "f5519676e9580060b510012ffde3b41dd5392a3debc98a2b02995499a086a7d4" default))
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(exordium-backup-files t)
 '(exordium-delete-trailing-whitespace nil)
 '(exordium-enable-y-or-n t)
 '(exordium-global-git-gutter-mode t)
 '(exordium-highlight-linum t)
 '(explicit-shell-file-name nil)
 '(filesets-data nil)
 '(flycheck-keymap-prefix "\3f")
 '(fringe-mode 16 nil (fringe))
 '(global-auto-revert-mode t)
 '(global-visual-line-mode nil)
 '(helm-ag-insert-at-point 'symbol nil nil "Customized with use-package helm-ag")
 '(helm-buffer-details-flag nil nil nil "Customized with use-package helm")
 '(helm-describe-function-function #'helpful-function nil nil "Customized with use-package helm")
 '(helm-describe-variable-function #'helpful-variable nil nil "Customized with use-package helm")
 '(helm-split-window-default-side 'other nil nil "Customized with use-package helm")
 '(imenu-max-item-length nil)
 '(imenu-sort-function 'imenu--sort-by-name)
 '(kept-new-versions 6)
 '(kill-whole-line t)
 '(line-move-visual nil)
 '(markdown-command "markdown_py")
 '(nyan-animate-nyancat t)
 '(nyan-mode t)
 '(org-support-shift-select t)
 '(package-selected-packages
   '(ac-js2 ac-rtags all-the-icons all-the-icons-nerd-fonts apache-mode
            auto-complete auto-complete-c-headers base bazel bison-mode
            bpftrace-mode caml casual cider cl-lib cl-libify claude-code-ide
            clojure-mode cmake-mode company company-rtags compat dap-mode
            dart-mode dash debian-el default-text-scale difftastic diminish
            docker dockerfile-mode dpkg-dev-el editorconfig eglot eldoc elm-mode
            enh-ruby-mode epl erc eval-sexp-fu evil exec-path-from-shell expand-region
            f faceup fill-column-indicator flycheck-package flycheck-pos-tip flycheck-relint
            flycheck-rtags flycheck-yamllint flymake flymake-yaml flymake-yamllint
            flymd flyover flyparens flyspell-lazy flyspell-popup forge fountain-mode
            git-gutter git-gutter-fringe git-modes git-timemachine gitlab-ci-mode gnuplot
            go-mode goto-chg graphviz-dot-mode groovy-mode haskell-mode helm-ag
            helm-descbinds helm-flycheck helm-lsp helm-projectile helm-rg helm-rtags
            helm-swoop helpful highlight highlight-symbol idlwave ido-completing-read+ iedit
            imenu-list impatient-mode inform-mode jinja2-mode js2-mode jsonrpc kivy-mode
            kotlin-mode lsp-ui lua-mode magit markdown-mode matlab-mode meson-mode modern-cpp-font-lock
            nginx-mode nlinum olivetti org-bullets org-superstar ox-gfm page-break-lines paredit
            peg php-mode pip-requirements pkg-info po-mode poly-rst powerline project
            projectile protobuf-mode puppet-mode python pyvenv qml-mode racket-mode rainbow-delimiters
            realgud rtags rust-mode s scala-mode symbol-overlay systemd track-changes tramp
            treemacs-projectile treemacs-projectileverilog-mode unfill vala-mode
            virtualenvwrapper vlf vterm vtm web-mode wgrep-helm which-key window-tool-bar
            ws-butler yaml-imenu yaml-mode yasnippet))
 '(package-vc-selected-packages
   '((claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")))
 '(protect-buffer-bury-p nil)
 '(python-shell-exec-path '("/opt/bb/bin"))
 '(python-shell-interpreter "python3.8")
 '(save-place-mode t)
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(tool-bar-mode nil)
 '(track-eol t)
 '(treemacs-git-mode nil)
 '(truncate-lines t)
 '(version-control t)
 '(word-wrap nil))
;;;;; '(make-backup-files t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cperl-array-face ((t (:foreground "yellow" :weight bold))))
 '(cperl-hash-face ((t (:foreground "Red" :slant italic :weight bold))))
 '(cursor ((t (:background "spring green"))))
 '(ediff-even-diff-A ((t (:foreground "#969896" :inverse-video nil))))
 '(ediff-even-diff-B ((t (:foreground "#969896" :inverse-video nil))))
 '(linum-highlight-face ((t (:background "white smoke" :distant-foreground "black")))))
;;; If we are running (not just displaying) the emacs process on a mac, we may
;;; want to set the default custom face to height 140.
