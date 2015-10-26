;;; prefs.el
;;;
;;; Overrides for init-prefs.el.
;;; Other overrides and preferences go before-init.el or after-init.el

(setq exordium-preferred-fonts '(
                                 ("Inconsolata" . 140)
                                 ("Consolas"    . 120)
                                 ("Monospace"   . 120)
                                 ("Mono"        . 120)
                                 ))

;;; Assist track eol
(setq exordium-line-mode 'nil)

;;; Don't auto-add )]} to ([{
(setq exordium-enable-electric-pair-mode nil)

;;; Turn off auto-complete...
(setq ac-auto-start nil)

;;; ...so I can summon it at a keystroke
;;; in init.el since the attribute to modify is
;;; not defined yet.
