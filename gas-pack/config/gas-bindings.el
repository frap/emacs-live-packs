;;Time-stamp: <2013-02-20 08:32:13 agasson>

;;(define-key global-map (kbd "M-3") (lambda () (interactive) (insert "#")))

;;(global-unset-key (kbd "M-3"))
;;(global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard)

;;Setup update of timestamp on file change - 1st eight lines of file : Time-stamp: <>
(add-hook 'before-save-hook 'time-stamp)

;; Org Gas Standard key bindings

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

;;(define-prefix-command 'f9)
;;(define-prefix-command 'f11)
;;(global-set-key (kbd "\C-cI")   'bh/punch-in)
;;(global-set-key (kbd "\C-cO")   'bh/punch-out)
;;(global-set-key (kbd "\C-c SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "C-c C-x C-i") 'org-pomodoro)
(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)

;;(global-set-key (kbd (f9)) 'org-clock-goto)
;;(global-set-key (kbd "<f11>") 'f11)

;;(define-key f9 (kbd "I") 'gas/clock-in)
;;(define-key f9 (kbd "O") 'gas/clock-out)
;;(define-key f9 (kbd "SPC") 'gas/clock-in-last-task)
;;(define-key <f11> 'org-clock-goto)
;;(define-key (kbd "\C") f11 'org-clock-in)
;;(define-key (kbd "\M" )f11 'org-resolve-clocks)
