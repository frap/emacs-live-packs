;; Wrapper for growlnotify
(defun growl-chat (title message &optional sticky)
  (interactive "sTitle: \nsGrowl: ")
  (shell-command
   (format "/usr/local/bin/growlnotify %s -m '%s' --appIcon 'Aquamacs Emacs' %s" title message (if sticky "--sticky" ""))))

;; Sticky notifications
(defun growl-chat-sticky (title message)
  (interactive "sTitle: \nsGrowl: ")
  (growl-chat title message t))
