;; Gas bindings
;;
;; Time-stamp: <2014-10-23 12:08:34 agasson>

;;(define-key global-map (kbd "M-3") (lambda () (interactive) (insert "#")))

;;(global-unset-key (kbd "M-3"))
;;(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

;;  ************* MACintosh *****************************
(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard)

;; ************** Org-mode ***************************

(global-set-key (kbd "C-c l")  'org-store-link)
(global-set-key (kbd "C-c a")  'org-agenda)
(global-set-key (kbd "C-c b")  'org-iswitchb)
(global-set-key (kbd "C-c g") 'gtd )

(global-set-key (kbd "<f1>")   'org-agenda)
(global-set-key (kbd "<f2>")   'org-clock-goto)
(global-set-key (kbd "C-<f2>") 'org-clock-in)

(global-set-key (kbd "C-c C-x C-i") 'gas/punch-in)
(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)
(global-set-key (kbd "C-c C-x C-p") 'org-pomodoro)
(global-set-key (kbd "\C-cI")       'gas/punch-in)
(global-set-key (kbd "\C-cO")       'gas/punch-out)


;; Respond Capture
(define-key global-map "\C-cr"
  (lambda () (interactive) (org-capture nil "r")))
;; TODO capture
(define-key global-map "\C-ct"
  (lambda () (interactive) (org-capture nil "t")))
;; MEETING capture - REUNION
(define-key global-map "\C-cm"
  (lambda () (interactive) (org-capture nil "m")))
;; PHONE Capture
(define-key global-map "\C-cp"
  (lambda () (interactive) (org-capture nil "p")))
;; NOTE capture
(define-key global-map "\C-cn"
  (lambda () (interactive) (org-capture nil "n")))
;; Journal capture
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))



;;(define-prefix-command 'f9)
;;(define-prefix-command 'f11)
;;(global-set-key (kbd "\C-c SPC") 'bh/clock-in-last-task)


;;(global-set-key (kbd (f9)) 'org-clock-goto)
;;(global-set-key (kbd "<f11>") 'f11)

;(define-key f2 (kbd "I") 'gas/punch-in)
;(define-key f2 (kbd "O") 'gas/punch-out)

;;(define-key f9 (kbd "SPC") 'gas/clock-in-last-task)
;;(define-key <f11> 'org-clock-goto)
;;(define-key (kbd "\C") f11 'org-clock-in)
;;(define-key (kbd "\M" )f11 'org-resolve-clocks)


;; ***************** ERC *****************************
;; switch to ERC with Ctrl+c e

(global-set-key (kbd "C-c e") 'gas-erc-start-or-switch) ;; ERC
