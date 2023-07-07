;;; before-init.el -- Summary

;;; Commentary:

;; My stuff, pre-Exordium

;;; Code:

(message "Start before-init.el")
(setq garbage-collection-messages 't )
;;;;(setq custom-file (concat "~/.emacs.d/taps/" (getenv "USER") "/emacs-custom.el"))
(cond
 ( (string-equal (getenv "BLOOMBERG") "true")
   (progn
     (message "Processing Bloomberg proxy requirements in before-init.el.")
     (defvar bbvpn-is-running "" "Is the Bloomberg network running.")
     (setq bbvpn-is-running
           (substring
            (shell-command-to-string "winproc-is-running -v  bbvpn2.exe")
            0 -1)
           )
     (defvar bb-nodeproxy-is-running "" "Is the Bloomberg proxier running.")
     (setq bb-nodeproxy-is-running
           (substring
            (shell-command-to-string "winproc-is-running -v  bb-nodeproxy-win32.exe")
            0 -1)
           )

     (cond
      ( ( and (string-equal (getenv "OS_ID") "WSL")
              (> (length bbvpn-is-running) 0)
              (> (length bb-nodeproxy-is-running) 0)
              )
        (progn
          (setq url-proxy-services
                '(
                  ("http"     . "localhost:8888")
                  ("https"    . "localhost:8888")))
          (message "Using WSL configs in before-init.el")
          ))
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
     )
   )
 )
(message "End before-init.el")
(provide `before-init)
;;; before-init ends here
