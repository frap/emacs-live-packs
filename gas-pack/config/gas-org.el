(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(require 'org-habit)
(require 'org-helpers)

;; POMODORO SETTINGS
;(add-to-list 'load-path (concat custom-emacs-dir "vendor/org-pomodoro"))
(require 'org-pomodoro)

(global-set-key (kbd "C-c C-x C-i") 'org-pomodoro)
(global-set-key (kbd "C-c C-x C-o") 'org-pomodoro)

(require 'weblock)

(add-hook 'org-pomodoro-started-hook
          '(lambda ()
             (weblock/toggle 'on)))

(add-hook 'org-pomodoro-finished-hook
          '(lambda ()
             (weblock/toggle 'off)))

(add-hook 'org-pomodoro-killed-hook
          '(lambda ()
             (weblock/toggle 'off)))

;; automatically mark a todo headline as done
;; when all sub-checkboxes are checked
(add-hook 'org-checkbox-statistics-hook 'oh/summary-todo-checkbox)

;; sets the default workflow keywords and their faces
(setq org-todo-keywords
      '((sequence "TODO(t)" "ENCOURS(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(o)" "|" "CANCELLED(c@/!)")))

(setq org-priority-faces
      '((65 :foreground "#ff7000" :weight bold)
        (66 :foreground "#ffa060" :weight bold)
        (67 :foreground "#ffcca8" :weight bold)))

(setq org-todo-keyword-faces
      '(("SOMEDAY"   :foreground "#808080" :weight bold)
        ("ENCOURS"      :foreground "#e9c062" :weight bold)
        ("STARTED"   :foreground "#ffff63" :weight bold)
        ("WAITING"   :foreground "#fd9b3b" :weight bold)
        ("HOLD"      :foreground "#9b859d" :weight bold)
        ("CANCELLED" :foreground "#9eb9a7" :weight bold)))

;; sets the
(setq org-tag-alist '((:startgroup . nil)
                      ("@maision" . ?m)
                      ("@bureau" . ?b)
                      ("@voiture" . ?v)
                      (:endgroup . nil)
                      ("org" . ?o)
                      ("git" . ?g)
                      ("prog" . ?p)
                      ("en ligne" . ?e)))

;; The default agenda files. inbox.org is used only in custom agenda.
(setq org-agenda-files (list "~/Dropbox/GTD/tasks.org"
                             "~/Dropbox/GTD/tasks.org_archive"
                             "~/Dropbox/GTD/projects.org"
                             "~/Dropbox/GTD/projects.org_archive"
                             "~/Dropbox/GTD/mevents.org"
                             "~/Dropbox/GTD/mevents.org_archive"
                             "~/Dropbox/GTD/calendar.org"))

;; my org settings
(custom-set-variables
 '(org-log-done t)
 '(org-completion-use-ido t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-ndays 1)
 '(org-agenda-include-diary t)
 '(org-agenda-window-setup 'current-window)
 '(org-agenda-repeating-timestamp-show-all t)
 ;; Show all agenda dates - even if they are empty
 '(org-agenda-show-all-dates t)
 ;; Sorting order for tasks on the agenda
 '(org-agenda-sorting-strategy
   (quote ((agenda
            habit-down
            time-up
            user-defined-up
            priority-down
            effort-up
            category-keep)
           (todo category-up priority-down effort-up)
           (tags category-up priority-down effort-up)
           (search category-up))))
 '(org-agenda-cmp-user-defined 'oh/agenda-sort)
 ;; Keep tasks with dates on the global todo lists
 '(org-agenda-todo-ignore-with-date nil)
 ;; Keep tasks with deadlines on the global todo lists
 '(org-agenda-todo-ignore-deadlines nil)
 ;; Keep tasks with scheduled dates on the global todo lists
 '(org-agenda-todo-ignore-scheduled nil)
 ;; Keep tasks with timestamps on the global todo lists
 '(org-agenda-todo-ignore-timestamp nil)
 ;; Remove completed deadline tasks from the agenda view
 '(org-agenda-skip-deadline-if-done t)
 ;; Remove completed scheduled tasks from the agenda view
 '(org-agenda-skip-scheduled-if-done t)
 ;; Remove completed items from search results
 '(org-agenda-skip-timestamp-if-done t)
 ;; Show lot sof clocking history so it's easy to pick items off the C-F11 list
 '(org-clock-history-length 36)
 ;; Separate drawers for clocking and logs
 '(org-drawers (quote ("PROPERTIES" "LOGBOOK")))
 ;; Save clock data and state changes and notes in the LOGBOOK drawer
 '(org-clock-into-drawer t)
 ;; Sometimes I change tasks I'm clocking quickly
 ;; this removes clocked tasks with 0:00 duration
 '(org-clock-out-remove-zero-time-clocks t)
 ;; Do not prompt to resume an active clock
 '(org-clock-persist-query-resume nil)
 ;; Include current clocking task in clock reports
 '(org-clock-report-include-clocking-task t)
 '(org-fast-tag-selection-single-key 'expert)
 '(org-agenda-skip-scheduled-if-done t)
 ;; Display tags farther right
 '(org-agenda-tags-column -102)
 ;; Enable display of the time grid
 ;; so we can see the marker for the current time
 '(org-agenda-time-grid (quote ((daily today remove-match)
                                #("----------------" 0 16 (org-heading t))
                                (830 1000 1200 1300 1500 1700))))
 ;; Do not dim blocked tasks
 '(org-agenda-dim-blocked-tasks nil))


(setq org-refile-targets '(("~/Dropbox/GTD/tasks.org" :level . 1)
                           ("~/Dropbox/GTD/projects.org" :level . 1)
                           ("~/Dropbox/GTD/references.org" :level . 1)
                           ("~/Dropbox/GTD/mevents.org" :level . 1)))

(setq org-capture-templates
      '(("r" "Todo" entry (file+headline "~/Dropbox/GTD/inbox.org" "Inbox")
         "* TODO %?")
        ("j" "Journal" entry (file+datetree "~/Dropbox/GTD/journal.org")
         (file "~/Dropbox/GTD/templates/review"))))

(define-key global-map "\C-cr"
  (lambda () (interactive) (org-capture nil "r")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(setq org-habit-graph-column 102)
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 21)

;; Some keybindings that should be activated in org-mode
(defun custom-org-agenda-mode-defaults ()
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer)
  (org-defkey org-agenda-mode-map "I" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "O" 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-o") 'org-pomodoro))

(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

;; configure org remember functions and hooks
(setq remember-annotation-functions '(org-remember-annotation)
      remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      '(("a" "Agenda"
         ((agenda "" nil)
          (tags-todo "-CANCELLED/!-HOLD-WAITING"
                     ((org-agenda-overriding-header "Projets Bloquès")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(non-project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :headline-if-restricted-and '(non-stuck-project)
                                        :subtree-if-unrestricted-and '(non-stuck-project)))))
          (tags-todo "-WAITING-CANCELLED/!ENCOURS"
                     ((org-agenda-overriding-header "Tâches Suivant")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-CANCELLED/!-ENCOURS-HOLD-WAITING"
                     ((org-agenda-overriding-header "Tâches Actif")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Les Projets actuellement actifs")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!WAITING|HOLD"
                     ((org-agenda-overriding-header "D'attente et reporté tâches")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit))))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/Dropbox/GTD/inbox.org"))))
        ("#" "Stuck Projects" tags-todo "-CANCELLED/!-HOLD-WAITING"
         ((org-agenda-overriding-header "Projets Bloqués")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                          habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "-WAITING-CANCELLED/!ENCOURS"
         ((org-agenda-overriding-header "Tâches Suivant")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-CANCELLED/!-ENCOURS-HOLD-WAITING"
         ((org-agenda-overriding-header "Tâches Actif")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(category-keep))))
        ("p" "Projects" tags-todo "-CANCELLED/!"
         ((org-agenda-overriding-header "Les Projets actuellement actifs")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
          (org-agenda-sorting-strategy '(category-keep))
          (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "-CANCELLED/!WAITING|HOLD"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
          (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))


(defun custom-org-mode-defaults ()
  (electric-indent-mode -1)
  (org-defkey org-mode-map (kbd "M-p") 'org-metaup)
  (org-defkey org-mode-map (kbd "M-n") 'org-metadown)
  (org-defkey org-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
  (org-defkey org-mode-map (kbd "C-c C-x C-o") 'org-pomodoro))


(add-hook 'org-mode-hook 'custom-org-mode-defaults)

(provide 'gas-org)
