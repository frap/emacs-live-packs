(defvar weblock/blocklist '("facebook"
                            "twitter"
                            "stumbleupon"
                            "coderwall"
                            "reader.google.com"
                            "9gag")
  "A list of services that should be blocked by weblocker.")

(defun weblock/blocklist-regexp ()
  (mapconcat 'identity weblock/blocklist "\\|"))

(defun weblock/toggle-line (&optional state)
  (interactive)
  (beginning-of-line)
  (if (re-search-forward (weblock/blocklist-regexp) (line-end-position) t)
      (progn
        (beginning-of-line)
        (if (looking-at "\\#")
            (unless (eq state 'off)
              (progn
                (delete-char 1)
                (while (looking-at " ")
                  (delete-char 1))))
          (unless (eq state 'on)
            (insert "# "))))))

(defun weblock/toggle (&optional state)
  "Opens /etc/hosts and toggles the "
  (find-file "/etc/hosts")
  (beginning-of-buffer)
  (loop do
        (weblock/toggle-line state)
        (forward-line 1)
        while (not (eobp)))
  (save-buffer)
  (kill-buffer (current-buffer)))

(provide 'weblock)
