(defun gas-popup (title msg &optional icon sound)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
of the message, MSG is the context. Optionally, you can provide an ICON and
a sound to be played"

  (interactive)
  (when sound (shell-command
                (concat "afplay " sound " 2> /dev/null")))
  (if (eq window-system 'x)
    (shell-command (concat "notify-send "

                     (if icon (concat "-i " icon) "")
                     " '" title "' '" msg "'"))
    ;; text only version

    (message (concat title ": " msg))))
