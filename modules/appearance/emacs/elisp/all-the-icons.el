(use-package all-the-icons)

(with-eval-after-load 'all-the-icons
  (with-eval-after-load 'magit
    (setq magit-format-file-function #'magit-format-file-all-the-icons)))
