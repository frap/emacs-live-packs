;; flyspell
      (setq-default ispell-program-name "/usr/bin/aspell")

;; flyspell in clojure mode. Only in comments
  (add-hook 'clojure-mode-hook
          (lambda ()
            (flyspell-prog-mode)))
