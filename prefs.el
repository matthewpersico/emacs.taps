;;; prefs.el
;;;
;;; Overrides for init-prefs.el. Called in init.el.
;;; Other overrides and preferences go before-init.el or after-init.el

(setq exordium-preferred-fonts '(
                                 ("Inconsolata" . 120)
                                 ("Consolas"    . 120)
                                 ("Monospace"   . 120)
                                 ("Mono"        . 120)
                                 ))

;;; Assist track eol
(setq exordium-line-mode 'nil)

;;; Don't auto-add )]} to ([{
(setq exordium-enable-electric-pair-mode nil)
