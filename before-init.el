;;; before-init.el

;; My stuff, pre-Exordium

(setq garbage-collection-messages 't )
(setq custom-file (concat "~/.emacs.d/taps/" (getenv "USER") "/after-init.el"))
(cond
 ((string-equal system-type "gnu/linux")
  (progn
    (setq url-proxy-services
          '(("no_proxy" . "^\\(localhost\\|10.*\\)")
            ("http"     . "devproxyfarm.bloomberg.com:82")
            ("https"    . "devproxyfarm.bloomberg.com:82")))))
 ((string-equal system-type "darwin")
  (progn
    (setq url-proxy-services
          '(("no_proxy" . "^\\(localhost\\|10.*\\)")
            ("http"     . "proxy.bloomberg.com:77")
            ("https"    . "proxy.bloomberg.com:77")))
    (message "Using darwin configs in before-init.el")
    ))
 )
