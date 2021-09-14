(use-package elfeed)

(use-package elfeed-org
  :after elfeed
  :custom
  (rmh-elfeed-org-tree-id "elfeed")
  (rmh-elfeed-org-ignore-tag "ignore")
  (rmh-elfeed-org-files '("@orgRoot@/rss/feeds.org"))
  :config
  (elfeed-org))

(use-package elfeed-goodies
  :after elfeed
  :config
  (elfeed-goodies/setup))

(use-package elfeed-dashboard
  :after elfeed
  :config
  (setq elfeed-dashboard-file "@orgRoot@/rss/elfeed-dashboard.org")
  (advice-add 'elfeed-search-quit-window :after #'elfeed-dashboard-update-links))

(use-package elfeed-score
  :after elfeed
  :bind
  (:map elfeed-search-mode-map
        ("=" . elfeed-score-map))
  :config
  (elfeed-score-enable))
