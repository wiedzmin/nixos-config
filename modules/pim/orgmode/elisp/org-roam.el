;; TODO: review/debug graph generation under v2
;; TODO: review manual at https://www.orgroam.com/manual.html
;; TODO: review https://github.com/search?q=org-roam-capture-templates&type=code
;; TODO: review https://github.com/search?q=org-roam-capture-ref-templates&type=code
;; TODO: review https://www.reddit.com/r/OrgRoam/comments/or31g0/a_few_qol_tricks_i_havent_seen_much_of_on_the/
;; TODO: review https://melpa.org/#/org-roam-ui
;; TODO: review https://melpa.org/#/org-roam-timestamps / https://github.com/ThomasFKJorna/org-roam-timestamps
;; TODO: review https://github.com/sanka047/dotfiles/blob/7d4fdab3d2534749a7bf5295c5d9eea6ff36e091/emacs/init.el#L530
(use-package org-roam
  :preface
  (defun custom/refile-to-roam ()
    "Moves current org subtree to org-roam node"
    (interactive)
    (save-excursion
      (org-cut-subtree)
      (org-roam-node-find t)
      (end-of-buffer)
      (org-yank)))
  :init
  (setq org-roam-v2-ack t)
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "@emacsOrgRoamPath@")
  (org-roam-graph-executable "@emacsOrgRoamDotBinary@")
  (org-roam-db-location "@emacsOrgRoamPath@/org-roam.db")
  (org-roam-tag-sources '(prop vanilla all-directories))
  (org-roam-verbose t)
  (org-roam-mode-sections
        (list #'org-roam-backlinks-section :unique t
              #'org-roam-reflinks-section
              #'org-roam-unlinked-references-section))
  (org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\n") :unnarrowed t)))
  :config
  @orgRoamAutosyncEnable@
  (use-package org-roam-protocol)
  :bind
  (:map org-roam-map
        ("a" . org-roam-alias-add)
        ("A" . org-roam-alias-remove)
        ("t" . org-roam-tag-add)
        ("T" . org-roam-tag-remove)
        ("l" . org-roam-ref-add)
        ("L" . org-roam-ref-remove)
        ("f" . org-roam-node-find)
        ("F" . org-roam-ref-find)
        ("d" . org-roam-dailies-find-directory)
        ("i" . org-roam-node-insert)
        ("c" . org-roam-capture)
        ("C" . custom/refile-to-roam)
        ("r" . org-roam-refile)
        ("R" . org-roam-node-random)
        ("x" . org-roam-extract-subtree)
        ("." . org-roam-demote-entire-buffer)
        ("," . org-roam-promote-entire-buffer)
        ("j" . org-roam-dailies-capture-today)))

(use-package org-roam-ui
  :custom
  (org-roam-ui-sync-theme t)
  (org-roam-ui-follow t)
  (org-roam-ui-update-on-save t)
  (org-roam-ui-open-on-start t))
