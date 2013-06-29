;; Solarized pack init file
;;
(live-load-config-file "bindings.el")

;; Load orglue
(live-add-pack-lib "orglue")
(require 'orglue)

;; Load org-octopress
(live-add-pack-lib "org-octopress")
(require 'org-octopress)
