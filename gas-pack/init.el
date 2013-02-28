;; Gas pack
;; Time-Stamp: <>
;; Load bindings config
(live-load-config-file "clean-mode-line.el")
(live-load-config-file "gas-org.el")
;;(live-load-config-file "gas-mobileorg.el")
(live-load-config-file "gas-irc.el")
(live-load-config-file "gas-sauron.el")
(live-load-config-file "gas-bindings.el")
(live-load-config-file "gas-spell.el")

;; Load alerts
;(live-add-pack-lib "sauron")
;(require 'sauron)

;; Load alerts
;(live-add-pack-lib "alert")
;(require 'alert)

;(defvar alert-default-style 'growl)
;(alert "This is an debugalert" :title "My Alert" :category 'debug)
;(alert "This is a SERIOUS alert" :category 'critical )

;(load-file (concat (live-pack-lib-dir) "gandalf.el"))

(require 'maxframe)
(setq mf-max-width 1200)  ;; Pixel width of main monitor.
(add-hook 'window-setup-hook 'maximize-frame t)

(load "server")
(unless (server-running-p) (server-start))
