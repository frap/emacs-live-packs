                                        ; Time-Stamp: <>

;; Mobile Org
(require 'org-mobile)
(setq org-mobile-inbox-for-pull "~/Dropbox/org/mobile.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(define-key org-mode-map "\C-cp" 'org-mobile-pull)
(define-key org-agenda-mode-map "\C-cp" 'org-mobile-pull)


(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/Dropbox/GTD/from-mobile.org")
