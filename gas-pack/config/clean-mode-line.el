(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (yas-global-mode . " γ")
    (undo-tree-mode . " τ")
    (paredit-mode . " φ")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (nrepl-mode . "ηζ")

    ;; Major modes
    (clojure-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "π")
    (emacs-lisp-mode . "EL")
    (nxhtml-mode . "nx"))
  "Alist for `clean-mode-line'.

When you add a new element to the alist, keep in mind that you
must pass the correct minor/major mode symbol and a string you
want to use in the modeline *in lieu of* the original.")

(defun clean-mode-line ()
  (interactive)
  (loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)
