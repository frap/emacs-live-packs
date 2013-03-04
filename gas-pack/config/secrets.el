;; 'silent to use symmetric encryption
;; nil to ask for users unless specified
;; t to always ask for a user
(setq epa-file-select-keys t)

;; secrets.el
(load-library "secrets.el.gpg")
(provide 'secrets)
