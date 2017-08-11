;; org-mode
(require 'org)
;; agenda view
(define-key global-map "\C-ca" 'org-agenda)
;; store link to document
(define-key global-map "\C-cl" 'org-store-link)
;; global transition states
(setq org-todo-keywords '("TODO" "STARTED" "WAITING" "VERIFY" "|" "DONE" "CANCELLED"))
;; effort estimate presets
'(org-refile-targets (quote (("gtd.org" :level . 2))))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; pull holidays and other events into agenda
(setq org-agenda-include-diary t)
;; include all unfinished todos in Org daily and weekly views
(setq org-agenda-include-all-todo t)
;; prompt for notes after tagging task as DONE
(setq org-log-done t)

;; Set to the location of your Org files on your local system
(setq org-directory "~/Documents")
;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/Documents/gtd.org")
;; set org-mobile staging area via dropbox
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;; files to sync
(setq org-mobile-files '("~/Documents/gtd.org"))

;; caputre tasks and journal entries in files
(setq org-default-notes-file "~/Documents/gtd.org")
(setq org-capture-templates
  '(("t" "Todo" entry (file+headline "~/Documents/gtd.org" "Tasks")
     "* TODO %?\n  %i\n  %a")
    ("j" "Journal" entry (file+datetree "~/Documents/journal.org")
     "* %?\nEntered on %U\n  %i\n  %a")))
(define-key global-map "\C-cc" 'org-capture)
