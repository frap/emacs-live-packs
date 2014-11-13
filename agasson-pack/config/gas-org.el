;; Gas Org Mode
;; Time-stamp: <2014-10-27 21:29:43 agasson>
;;

(require 'org)
;; load org helper libraries
(require 'org-habit)
(live-add-pack-lib "express")
(live-add-pack-lib "org-helpers")

;;(require 'gas-clocking)

;; org files
(add-to-list 'load-path (expand-file-name "~/Dropbox/GTD"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)"))

;; setup for org-capture
(setq org-directory "~/Dropbox/GTD")
(setq org-default-notes-file "~/Dropbox/GTD/inbox.org")

(defun gtd ()
  (interactive)
  (find-file "/home/agasson/Dropbox/GTD/gtd.org"))

;; The default agenda files. inbox.org is used only in custom agenda.
(setq org-agenda-files (quote ( "~/Dropbox/GTD/gtd.org"
                                "~/Dropbox/GTD/goals.org"
                                "~/Dropbox/GTD/career.org"
                                "/home/agasson/Dropbox/GTD/feedme.org"
                                "~/Dropbox/GTD/calendar.org"
                                "~/Dropbox/GTD/google-cal.org")))

;;Set up Habit graphs
;; display the tags farther right
(setq org-agenda-tags-column -62)
;; display the org-habit graph right of the tags
(setq org-habit-graph-column 62)
(setq org-habit-following-days 7)
(setq org-habit-preceding-days 14)

;; Set up abbreviations
(setq org-link-abbrev-alist
       '(("bugzilla"  . "http://10.1.2.9/bugzilla/show_bug.cgi?id=")
         ("url-to-fr" . "http://translate.google.fr/translate?sl=en&tl=fr&u=%h")
         ("google"    . "http://www.google.com/search?q=")
         ("gmap"      . "http://maps.google.com/maps?q=%s")
         ("omap"      . "http://nominatim.openstreetmap.org/search?q=%s&polygon=1")
         ("ads"       . "http://adsabs.harvard.edu/cgi-bin/nph-abs_connect?author=%s&db_key=AST")
         ("ghub"      : "http://github.com/%s")
         ("ghub-pages" : "http://%s.github.io")
         ))

;; sets the default workflow keywords and their faces
(setq org-todo-keywords
      '((sequence "TODO(t)" "EN_COURS(e)" "|" "FINI(f!/!)")
        (sequence "RENDEZ_VOUS(r)" "TÉLÉPHONE(p)" "|" "FINI(f!/!)")
        (sequence "|" "ANNULÉ(a@/!)")
        (sequence "GOAL(G)" "DAEMONS(D)" "QUOTE(Q)")
        (sequence "ATTENTE(w@/!)" "SOUTE(h@/!)" "UN_JOUR(j)" "VALUE(V)")))

(setq org-priority-faces
      '((65 :foreground "#ff2f30" :weight bold)
        (66 :foreground "#ffaf60" :weight bold)
        (67 :foreground "#00dca8" :weight bold)))

(setq org-todo-keyword-faces
      '(("UN_JOUR"       :foreground "#c93f80" :weight bold)
        ("EN_COURS"      :foreground "#2f2ccc" :weight bold)
        ("ATTENTE"       :foreground "#fd9b3b" :weight bold)
        ("FINI"          :foreground "#19b85d" :weight bold)
        ("SOUTE"         :foreground "#afff64" :weight bold)
        ("ANNULÉ"        :foreground "#b81590" :weight bold)
        ("TÉLÉPHONE"     :foreground "#2eb9a7" :weight bold)
        ("GOAL"          :foreground "#1010ff" :weight bold)
        ("VALUE"         :foreground "#afff10" :weight bold)
        ("QUOTE"         :foreground "#146290" :weight bold)
        ("DAEMONS"       :foreground "#b46230" :weight bold)
        ("RENDEZ_VOUS"   :foreground "#0f4f43" :weight bold)
        ))

; change TODO face
(set-face-attribute 'org-todo nil
                    :weight 'bold :box '(:line-width 1 :color "#D80000")
                                        :foreground "#D80000" :background "#000000")

;; sets the TAG list
(setq org-tag-alist '((:startgroup . nil)
                      ("@maision" . ?m)
                      ("@bureau" . ?b)
                      ("@voiture" . ?v)
                      ("@ferme"   . ?f)
                      (:endgroup . nil)
                      ("TÉLÉPHONE" . ?t)
                      ("RENDEZ_VOUS" . ?r)
                      ("ATTENTE" . ?w)
                      ("SOUTE" . ?h)
                      ("PERSONAL" . ?P)
                      ("WORK" . ?W)
                      ("FERME" . ?F)
                      ("ORG" . ?O)
                      ("crypt" . ?c)
                      ("NOTE" . ?n)
                      ("ANNULÉ" . ?a)
                      ("GIT" . ?g)
                      ("PROG" . ?p)
                      ("en ligne" . ?e)))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/Dropbox/GTD/inbox.org" "Boîte de réception")
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "répondre" entry (file+headline "~/Dropbox/GTD/inbox.org" "Répondre")
         "* EN_COURS Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
        ("n" "commentaire" entry (file+headline "~/Dropbox/GTD/inbox.org" "Commentaire")
                        "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("j" "journal" entry (file+datetree "~/Dropbox/GTD/journal.org")
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file+headline "~/Dropbox/GTD/inbox.org" "Interruptions")
         "* RENDEZ_VOUS avec %? :MEETING:\n%U" :clock-in t :clock-resume t)
        ("p" "appel téléphonique" entry (file+headline "~/Dropbox/GTD/inbox.org" "Interruptions")
         "* TÉLÉPHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
        ("h" "Habitude" entry (file "~/Dropbox/GTD/inbox.org")
                        "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: EN_COURS\n:END:\n")
        ))
