;;;
;;; Gas Org Mode
;;;
;(load-file "todochiuku.el")

(require 'org)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;(require 'pomodoro)
;(pomodoro-add-to-mode-line)
;(global-set-key (kbd "C-c C-x C-p") 'pomodoro-start)
;(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)

;; (require 'weblock)

;; (add-hook 'org-pomodoro-started-hook
;;           '(lambda ()
;;              (weblock/toggle 'on)))

;; (add-hook 'org-pomodoro-finished-hook
;;           '(lambda ()
;;              (weblock/toggle 'off)))

;; (add-hook 'org-pomodoro-killed-hook
;;           '(lambda ()
;;              (weblock/toggle 'off)))

(setq org-link-abbrev-alist
       '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
         ("url-to-ja" . "http://translate.google.fr/translate?sl=en&tl=ja&u=%h")
         ("google"    . "http://www.google.com/search?q=")
         ("gmap"      . "http://maps.google.com/maps?q=%s")
         ("omap"      . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
         ("ads"       . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")
         ("ghub"      : "http://github.com/%s")
         ("ghub-pages" : "http://%s.github.io")
         ))

(provide 'gas-org)
