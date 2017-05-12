;;; before-init.el

;; My stuff, pre-Exordium

(setq garbage-collection-messages 't )
(setq custom-file (concat "~/.emacs.d/taps/" (getenv "USER") "/after-init.el"))
(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http"     . "devproxyfarm.bloomberg.com:82")
        ("https"    . "devproxyfarm.bloomberg.com:82")))
