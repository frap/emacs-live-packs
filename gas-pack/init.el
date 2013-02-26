;; Gas pack
;; Time-Stamp: <>
;; Load bindings config
(live-load-config-file "clean-mode-line.el")
(live-load-config-file "gas-org.el")
;(live-load-config-file "gas-mobileorg.el")
(live-load-config-file "gas-bindings.el")
(live-load-config-file "gas-spell.el")



;; use zenburn as the default theme
;(load-theme 'zenburn t)

;(load-file (concat (live-pack-lib-dir) "gandalf.el"))

(require 'maxframe)
(setq mf-max-width 1200)  ;; Pixel width of main monitor.
(add-hook 'window-setup-hook 'maximize-frame t)

(load "server")
(unless (server-running-p) (server-start))
