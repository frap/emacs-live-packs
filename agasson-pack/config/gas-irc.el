(provide 'gas-irc)

(require 'erc)

;; joining && autojoing

;; make sure to use wildcards for e.g. freenode as the actual server
;; name can be be a bit different, which would screw up autoconnect
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
  '((".*\\.freenode.net" "#emacs" "#clojure" "#twisted" "#twisted-web")))
;;     (".*\\.gimp.org" "#unix" "#gtk+")))

;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"

                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun gas-erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "frap" :full-name "cisco123"))))
;;      (erc :server "irc.gimp.org" :port 6667 :nick "foo" :full-name "bar"))))


(defun be-social ()
  "Connect to IM networks using bitlbee."
  (interactive)
  (erc :server "localhost" :port 6667 :nick "frap"))

(add-hook 'erc-join-hook 'bitlbee-identify)
(defun bitlbee-identify ()
   "If we're on the bitlbee server, send the identify command to the
 &bitlbee channel."
   (when (and (string= "localhost" erc-session-server)
              (string= "&bitlbee" (buffer-name)))
     (erc-message "PRIVMSG" (format "%s identify %s"
                                    (erc-default-target)
                                    gas-bitlbee-password))))
