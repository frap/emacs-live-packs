;;;
;;; Org mode-alist
;;;
(add-to-list 'load-path (expand-file-name "~/opt/local/share/emacs/site-lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org-habit)
(require 'org-helpers)
(require 'org-pomodoro)
;(require 'org-install)
;;  significant functionality depends on font-locking being active, is default
(global-font-lock-mode 1)
;; if activate to I get more errors?
(setq debug-on-error 1)

;;; Org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . nil)
   (ditaa . nil)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . nil)
   (haskell . nil)
   (ocaml . nil)
   (python . t)
   (ruby . nil)
   (screen . nil)
   (sh . t)
   (sql . t)
   (sqlite . t)))

;; org link abbreviations
(setq org-link-abbrev-alist
      '(("bugzilla". "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
        ("google"  . "http://www.google.com/search?q=")
        ("gmap"    . "http://maps.google.com/maps?q=%s")
        ("omap"    . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
        ("ads"     . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")
        ("edcs"    . "http://wwwin-eng.cisco.com/protected-cgi-bin/edcs/edcs_attr_search.pl?doc_num=%s")
        ("ddts"    . "http://cdetsweb-prd.cisco.com/apps/dumpcr?identifier=%s")
        ("rfc"     . "http://tools.ietf.org/html/rfc%s")
        ("draft"   . "http://tools.ietf.org/html/draft-%s")
        ))

;; Enable abbrev-mode
(add-hook 'org-mode-hook (lambda () (abbrev-mode 1)))

;;; Apple Mail integration
;;(add-to-list 'org-mac-message)
;;(setq org-modules (quote (org-mac-message)))
;;(setq org-mac-mail-account "Local")

;; Mac mail integration with flagged messages

;;(defun mail-import ()
;;  (let ((org-mac-mail-account "Local"))
;;    (org-mac-message-insert-flagged "gtd.org" "Flagged mail")))

;; Enable Org habits

;(add-to-list 'org-modules 'org-habit)
;; display teh tags farther right
(setq org-agenda-tags-column -102)
;; display the org-habit graph right of the tags
(setq org-habit-graph-column 102)
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 21)
;(setq org-clock-sound "~/Music/sounds/tos-redalert.mp3")



;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
(add-hook 'org-mode-hook
          (lambda ()
            ;; yasnippet
            (make-variable-buffer-local 'yas/trigger-key)
            (org-set-local 'yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-group)
            ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
            ;; It expands the files in the directories individually
            (flyspell-mode 1)
            ;; auto-fill mode on
            (auto-fill-mode 1)
            (org-defkey org-mode-map "\C-c["    'undefined)
            (org-defkey org-mode-map "\C-c]"    'undefined)
            (local-set-key (kbd "C-c M-o") 'gas/mail-subtree)) 'append)

(defun gas/mail-subtree ()
  (interactive)
  (org-mark-subtree)
  (org-mime-subtree))

(let ((langs '("american" "francais" "espanol")))
      (setq lang-ring (make-ring (length langs)))
      (dolist (elem langs) (ring-insert lang-ring elem)))

(defun cycle-ispell-languages ()
      (interactive)
      (let ((lang (ring-ref lang-ring -1)))
        (ring-insert lang-ring lang)
        (ispell-change-dictionary lang)))
(global-set-key [f6] 'cycle-ispell-languages)
;; flyspell mode for spell checking everywhere
;;(add-hook 'org-mode-hook 'turn-on-flyspell 'append)


(defun org-quote-footnote-field (s)
  "Quote footnote field for inclusion in Wiki material."
;;  (if (string-match "[fn\:,]" s)
;;      (concat "\"" (mapconcat 'identity (split-string s "\"") "\"\"") "\"")
       (replace-regexp-in-string "fn:" "#fn" s )
    )

;; set up formatting of wiki
(defun orgtbl-to-wiki (table params)
  "Convert the orgtbl-mode table to Cisco Wiki material.
This does take care of the proper quoting of fields with comma or quotes."
  (orgtbl-to-generic table (org-combine-plists
                            '(:sep "|" :lstart "|" :hlsep "||" :lend "|"
                :hlstart "||" :hlend "||" :fmt org-quote-footnote-field)
                            params)))

(setq org-agenda-files (quote ("~/Dropbox/GTD/gtd.org"
                               "~/Dropbox/GTD/inbox.org"
                               "~/Dropbox/GTD/journal.org"
                               "~/Dropbox/GTD/projects.org"
                               "~/Dropbox/GTD/quote.org"
                               "~/Dropbox/GTD/calendar.org"
                               "~/Dropbox/GTD/birthday.org"
                               "~/Dropbox/GTD/gbell.org"
                               )))
;; Export to CSV by default
(setq org-table-export-default-format "orgtbl-to-csv")

(setq org-log-done nil)
(setq org-agenda-include-diary nil)
;; Follow links in documents via RETURN
(setq org-return-follows-link t)
(setq org-deadline-warning-days 7)
(setq org-timeline-show-empty-dates t)

;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
'(highlight ((t (:background "blue"))))
'(hl-line ((t (:inherit highlight :background "darkseagreen"))))

;; change TODO selection fast
(setq org-use-fast-todo-selection t)
;; to use changing todo states with S-left and S-right
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

;; TODO State filters (adds TAGs on state change)
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("ENCOURS" ("WAITING"))
              ("COMMENCÉ" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED")))))

;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date nil)

;; Allow deadlines which are due soon to appear on the global todo lists
(setq org-agenda-todo-ignore-deadlines (quote far))

;; Keep tasks scheduled in the future off the global todo lists
(setq org-agenda-todo-ignore-scheduled (quote future))

;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)

;;(setq org-agenda-todo-ignore-deadlines t)
;;(setq org-agenda-todo-ignore-scheduled t)

(setq org-insert-mode-line-in-empty-file t)

; Use IDO for target completion
(setq org-completion-use-ido t)

; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))

; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;(setq org-refile-targets (quote (("gtd.org" :maxlevel . 1)
;;                               ("refile.org" :maxlevel . 2))))

(setq org-agenda-sorting-strategy
      (quote
       ((agenda habit-down time-up priority-down effort-up category-keep)
        (todo priority-down effort-up category-keep)
        (tags priority-down category-keep)
        (search category-keep))))

(setq org-agenda-exporter-settings
      '((ps-number-of-columns 1)
        (ps-landscape-mode t)
        (htmlize-output-type 'css)))

;; skip text before 1st headline on export
(setq org-export-skip-text-before-1st-heading t)

;; TODO Keywords
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "ENCOURS(n)" "COMMENCÉ(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "SOMEDAY(S!)" "|" "CANCELLED(c@/!)")
              (sequence "RUN(R!)" "BIKE(B!)" "SWIM(M!)" "|" "DONE(d!/!)")
              (sequence "OPEN(O!)" "|" "CLOSED(C!)"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("ENCOURS" :foreground "blue" :weight bold)
              ("COMMENCÉ" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("RUN" :foreground "yellow" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))


(setq org-tag-alist '((:startgroup)
                      ("@office" . ?w) ("@home" . ?h) ("@ordenador" . ?o) ("@errand" . ?e) ("@farm" . ?f)
                      (:endgroup)
                      ("LEER" . ?l) ("PHONE" . ?p) ("PHRASE" . ?P) ("VERB" . ?V) ("NOUN" . ?N)
                      ("ADJECTIVE" . ?A) ("SLANG" . ?S) ("KOAN" . ?K) ("NOTE" . ?n)
                      ("QUOTE" . ?Q) ("CANCELLED" . ?C) ("IM" . ?i) ("REFILE" .?R)))
; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; %a captures link where called command from so when you do C-C C-c return to there
(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/Dropbox/GTD/inbox.org")
               "* TODO %?\n%U\n%a\n  %i" :clock-in t :clock-resume t)
        ("a" "Appointment" entry (file+headline "~/Dropbox/GTD/calendar.org" "Calendar")
         "* APPT %^{Appt Description} %^g\n %?")
        ("i" "Interruption" entry (file "~/Dropbox/GTD/inbox.org")
             "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("q" "Quote" entry (file+headline "~/Dropbox/GTD/quote.org" "Quotes")
             "* QUOTE %^{Quote} %^g\n:PROPERTIES:\n:author: %?\n:END:")
        ("s" "Spanish Phrase" entry (file+headline "~/Dropbox/GTD/quote.org" "Spanish")
             "* WORD %^{Word} %^g\n:PROPERTIES:\n:created: %U\n:translation: %?\n:END:")
        ("f" "French Phrase" entry (file+headline "~/Dropbox/GTD/quote.org" "French")
             "* WORD %^{Word} %^g\n:PROPERTIES:\n:created: %U\n:translation: %?\n:END:")
        ("n" "Note" entry (file "~/Dropbox/GTD/inbox.org")
             "* %? :NOTE:\n%U\n%a\n %i" :clock-in t :clock-resume t)
        ("h" "Habit" entry (file+headline "~/Dropbox/GTD/gtd.org" "Habits")
         "* ENCOURS %?\n%U\n%a\nSCHEDULED: %t .+1d/3d>\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: ENCOURS\n:END:\n  %i")
        ("j" "Journal" entry (file+datetree "~/Dropbox/GTD/journal.org")
         (file "~/Dropbox/GTD/templates/review"))
        ))

;;%U = current timestamp %? = put cursor %i initial content, %a annotation aka link, %i includes any kill ring buffer

;; Remove empty LOGBOOK drawers on clock out
(defun bh/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook 'bh/remove-empty-drawer-on-clock-out 'append)

(defun gtd ()
  (interactive)
  find-file "~/Dropbox/GTD/gtd.org")
(global-set-key (kbd "C-c g") 'gtd )


(setq org-directory "~/Dropbox/GTD/")
(setq org-notes-default-file (concat org-directory "/inbox.org"))

; Targets include this file and any file contributing to the agenda - up to 2 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 3)
                                 (org-agenda-files :maxlevel . 3))))

; Stop using paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path nil)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

; Use IDO for both buffer and file completion and ido-everywhere to t
(setq org-completion-use-ido t)
(setq ido-everywhere t)
(setq ido-max-directory-size 100000)
(ido-mode (quote both))

;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

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

(setq org-agenda-custom-commands
      '(("a" "Agenda"
         ((agenda "" nil)
          (alltodo ""
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-agenda-files '("~/Dropbox/GTD/inbox.org"))
                    (org-agenda-skip-function
                     '(oh/agenda-skip :headline-if-restricted-and '(todo)))))
          (tags-todo "-CANCELLED/!-HOLD-WAITING"
                     ((org-agenda-overriding-header "Stuck Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project habit scheduled deadline)))))
          (tags-todo "-WAITING-CANCELLED/!ENCOURS"
                     ((org-agenda-overriding-header "Tasks En Cours")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
                      (org-tags-match-list-sublevels t)
                      (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
          (tags-todo "-CANCELLED/!-ENCOURS-HOLD-WAITING"
                     ((org-agenda-overriding-header "Available Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :headline-if '(project)
                                        :subtree-if '(inactive habit scheduled deadline)
                                        :subtree-if-unrestricted-and '(subtask)
                                        :subtree-if-restricted-and '(single-task)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!"
                     ((org-agenda-overriding-header "Currently Active Projects")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(non-project stuck-project inactive habit)
                                        :headline-if-unrestricted-and '(subproject)
                                        :headline-if-restricted-and '(top-project)))
                      (org-agenda-sorting-strategy '(category-keep))))
          (tags-todo "-CANCELLED/!WAITING|HOLD"
                     ((org-agenda-overriding-header "Waiting and Postponed Tasks")
                      (org-agenda-skip-function
                       '(oh/agenda-skip :subtree-if '(project habit))))))
         nil)
        ("r" "Tasks to Refile" alltodo ""
         ((org-agenda-overriding-header "Tasks to Refile")
          (org-agenda-files '("~/Dropbox/GTD/inbox.org"))))
        ("#" "Stuck Projects" tags-todo "-CANCELLED/!-HOLD-WAITING"
         ((org-agenda-overriding-header "Stuck Projects")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive non-project non-stuck-project
                                          habit scheduled deadline)))))
        ("n" "Tasks Proximos" tags-todo "-WAITING-CANCELLED/!ENCOURS"
         ((org-agenda-overriding-header "Tasks Proximos")
          (org-agenda-skip-function
           '(oh/agenda-skip :subtree-if '(inactive project habit scheduled deadline)))
          (org-tags-match-list-sublevels t)
          (org-agenda-sorting-strategy '(todo-state-down effort-up category-keep))))
        ("R" "Tasks" tags-todo "-CANCELLED/!-ENCOURS-HOLD-WAITING"
         ((org-agenda-overriding-header "Available Tasks")
          (org-agenda-skip-function
           '(oh/agenda-skip :headline-if '(project)
                            :subtree-if '(inactive habit scheduled deadline)
                            :subtree-if-unrestricted-and '(subtask)
                            :subtree-if-restricted-and '(single-task)))
          (org-agenda-sorting-strategy '(category-keep))))
        ("p" "Projects" tags-todo "-CANCELLED/!"
         ((org-agenda-overriding-header "Currently Active Projects")
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

;; disabeldisplay of child tasks
;(setq org-tags-match-list-sublevels nil)

;; Start of clocking config

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Small windows on my Eee PC displays only the end of long lists which isn't very useful
(setq org-clock-history-length 10)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task to COMMENCÉ when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-started)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist (quote history))
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Agenda log mode items to display (clock time only by default)
(setq org-agenda-log-mode-items (quote (clock)))
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:15 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00 8:"))))


;; Archiving
(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(defun bh/skip-non-archivable-tasks ()
  "Skip trees that are not available for archiving"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
    ;; Consider only tasks with done todo headings as archivable candidates
    (if (member (org-get-todo-state) org-done-keywords)
        (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
               (daynr (string-to-int (format-time-string "%d" (current-time))))
               (a-month-ago (* 60 60 24 (+ daynr 1)))
               (last-month (format-time-string "%Y-%m-" (time-subtract (current-time) (seconds-to-time a-month-ago))))
               (this-month (format-time-string "%Y-%m-" (current-time)))
               (subtree-is-current (save-excursion
                                     (forward-line 1)
                                     (and (< (point) subtree-end)
                                          (re-search-forward (concat last-month "\\|" this-month) subtree-end t)))))
          (if subtree-is-current
              next-headline ; Has a date in this month or last month, skip it
            nil))  ; available to archive
      (or next-headline (point-max)))))
