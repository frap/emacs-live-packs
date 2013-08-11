;; Gas pack
;; Time-stamp: <2013-07-10 15:30:58 agasson>
;; Load bindings config

(require 'package)

;; inactivate any proxy (got some trouble with authentication)
(setq url-proxy-services '(("no_proxy" . "red-elvis\\.net")))

(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)

;; a utility function to help in installing emacs packages
(defun install-packs (packs)
  (progn
    (when (not package-archive-contents)
      (package-refresh-contents))
    (dolist (p packs)
      (when (not (package-installed-p p))
        (package-install p)))))

;; Time-stamp hook for saving alst file saved time
(setq time-stamp-pattern nil)
(add-hook 'before-save-hook 'time-stamp)

(live-load-config-file "clean-mode-line.el")
(live-load-config-file "secrets.el")
(live-load-config-file "gas-org.el")
;;(live-load-config-file "gas-mobileorg.el")
(live-load-config-file "gas-irc.el")
(live-load-config-file "gas-sauron.el")
(live-load-config-file "gas-bindings.el")
(live-load-config-file "gas-spell.el")

;; Load ctable
(live-add-pack-lib "ctable")
(require 'ctable)
;; Load sauron
(live-add-pack-lib "sauron")
(require 'sauron)

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
