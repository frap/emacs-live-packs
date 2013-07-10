;; 'silent to use symmetric encryption
;; nil to ask for users unless specified
;; t to always ask for a user
(require 'epa-file)
(epa-file-enable)

(setq epa-file-select-keys nil)

;; secrets.el
(load-library "secrets.el.gpg")
(provide 'secrets)
