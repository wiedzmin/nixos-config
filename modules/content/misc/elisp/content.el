(use-package elfeed
  :preface
  (defun custom/elfeed-search-browse-url-eww ()
    "Visit the current entry in your browser with EWW. This is
literally a clone of `elfeed-search-browse-url' with necessary
changes, because there seems to be no existing functionality at
need, or at least it is hard to find."
    (interactive)
    (let ((buffer (current-buffer))
          (entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (eww-browse-url it))
      ;; `browse-url' could have switched to another buffer if eww or another
      ;; internal browser is used, but the remainder of the functions needs to
      ;; run in the elfeed buffer.
      (with-current-buffer buffer
        (mapc #'elfeed-search-update-entry entries)
        (unless (or elfeed-search-remain-on-entry (use-region-p))
          (forward-line)))))
  (defun custom/elfeed-filter-by-ordinality ()
    "was taken from https://takeonrules.com/2025/03/01/reorganizing-my-rss-feed-reading/
!!! DO NOT REMOVE !!!
This function should be kept as a reference until gaining more deep understanding
of tagging semantics, scoring and all related things.

Then actual filtering meta should be actualized and maybe
implementation will also change.

Select a default filter and update elfeed.
"
    (interactive)
    (let* ((filters
            '(("First (1st)" . "+1st +unread")
              ("Second (2nd)" . "@7-days-ago +2nd +unread")
              ("Third (3rd)" . "@7-days-ago +3rd +unread")
              ("Outlets" . "@2-days-ago +outlet")))
           (filter
            (completing-read "Elfeed Filter: " filters nil t)))
      (setq elfeed-search-filter
            (alist-get filter filters nil nil #'string=))
      (elfeed-search-update :force)))
  (defun custom/elfeed-search-tag-all ()
    "Apply TAG to all selected entries."
    (interactive)
    (elfeed-search-tag-all
      (intern (completing-read "Tag: "
                  (elfeed-db-get-all-tags)))))
  (defun custom/elfeed-show-tag ()
    "Add TAGS to the displayed entry."
    (interactive)
    (apply #'elfeed-show-tag
      (mapcar
        #'intern
        (completing-read-multiple "Tag(s): "
          (elfeed-db-get-all-tags)))))
  (defun custom/elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer."
    ;;write to disk when quitting
    (interactive)
    (elfeed-db-save)
    (quit-window))
  (defun custom/elfeed ()
    "Load the elfeed db from disk before opening."
    (interactive)
    (elfeed)
    (elfeed-update)
    (elfeed-db-load)
    (elfeed-search-update--force))
  :init
  (use-package elfeed-goodies)
  (use-package elfeed-org)
  (use-package elfeed-score)
  :bind
  (:map elfeed-search-mode-map
        ("e" . custom/elfeed-search-browse-url-eww)
        ("+" . custom/elfeed-search-tag-all)
        ("q" . custom/elfeed-save-db-and-bury))
  (:map elfeed-show-mode-map
        ("y" . nil)
        ("y y" . elfeed-show-yank)
        ("+" . custom/elfeed-show-tag)
        ("-" . elfeed-show-untag))
  :custom
  (elfeed-curl-timeout 90)
  (elfeed-user-agent url-user-agent)
  (elfeed-db-directory "@emacsVarDir@/elfeed/db/")
  (rmh-elfeed-org-tree-id "elfeed")
  (rmh-elfeed-org-ignore-tag "ignore")
  (rmh-elfeed-org-files '("@orgRoamRoot@/feeds.org"))
  (elfeed-score-serde-score-file "@emacsEtcDir@/elfeed/score/score.el")
  (elfeed-search-print-entry-function #'elfeed-score-print-entry)
  (elfeed-search-sort-function #'elfeed-score-sort)
  :config
  (define-key elfeed-search-mode-map "=" elfeed-score-map) ; NOTE: because keymap is not function
  (elfeed-goodies/setup)
  (elfeed-org)
  (elfeed-score-enable))
