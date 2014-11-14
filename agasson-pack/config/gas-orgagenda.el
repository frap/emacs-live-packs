;;; gas-orgagenda.el --- My Org-Agenda settings
;; Time-stamp: <2014-09-22 12:58:48 agasson>

(require 'org-helpers)

(defun custom-org-agenda-mode-defaults ()
  (electric-indent-mode -1)
  (org-defkey org-agenda-mode-map "W" 'oh/agenda-remove-restriction)
  (org-defkey org-agenda-mode-map "N" 'oh/agenda-restrict-to-subtree)
  (org-defkey org-agenda-mode-map "P" 'oh/agenda-restrict-to-project)
  (org-defkey org-agenda-mode-map "q" 'bury-buffer)
  (org-defkey org-agenda-mode-map "I" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "O" 'org-pomodoro)
  (org-defkey org-agenda-mode-map "p" 'gas/punch-in)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-i") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-o") 'org-pomodoro)
  (org-defkey org-agenda-mode-map (kbd "C-c C-x C-p") 'gas/punch-in))

(add-hook 'org-agenda-mode-hook 'custom-org-agenda-mode-defaults 'append)

(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda "" nil)
          (alltodo ""
                   ((org-agenda-overriding-header "Tâches à la Représenter")
                    (org-agenda-files '("~/Dropbox/GTD/inbox.org"))
                    (org-agenda-skip-function
                     '(oh/agenda-skip :headline-if-restricted-and '(todo)))))
          (tags-todo "-ANNULÉ/!-SOUTE-ATTENTE-GOAL"
                     ((org-agenda-overriding-header "Projets Bloqués")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project habit scheduled deadline)))))
          (tags-todo "-ATTENTE-ANNULÉ/!EN_COURS"
                     ((org-agenda-overriding-header "Tâches à Venir")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(priority-down todo-state-down effort-up category-keep))))
          (tags-todo "-ANNULÉ/!-EN_COURS-SOUTE-ATTENTE-VALUE-GOAL"
                     ((org-agenda-overriding-header "Tâches Disponibles")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(effort-up priority-down))))
          (tags-todo "-ANNULÉ/!"
                     ((org-agenda-overriding-header "Projets actuellement Actifs")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(effort-up priority-down category-keep))))
          (tags-todo "-ANNULÉ/!ATTENTE|SOUTE"
                     ((org-agenda-overriding-header "Attente ou Reporté Tâches")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit))))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/.org/inbox.org"))))
        ("#" "Stuck Projects" tags-todo "-ANNULÉ/!-SOUTE-ATTENTE"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                                   habit scheduled deadline)))))
        ("n" "Next Tasks" tags-todo "-ATTENTE-ANNULÉ/!EN_COURS"
         ((org-agenda-overriding-header "Next Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-ANNULÉ/!-EN_COURS-SOUTE-ATTENTE"
         ((org-agenda-overriding-header "Available Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(category-keep))))
        ("p" "Projects" tags-todo "-ANNULÉ/!"
         ((org-agenda-overriding-header "Currently Active Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(non-project inactive habit)))
          (org-agenda-sorting-strategy '(category-keep))
          (org-tags-match-list-sublevels 'indented)))
        ("w" "Waiting Tasks" tags-todo "-ANNULÉ/!ATTENTE|SOUTE"
         ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                    (org-agenda-skip-function '(oh/agenda-skip :subtree-if '(project habit)))))))

;; refiling
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
