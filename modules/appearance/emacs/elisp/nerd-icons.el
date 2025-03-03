(use-package nerd-icons)

(with-eval-after-load 'nerd-icons
  (with-eval-after-load 'magit
    (setq magit-format-file-function #'magit-format-file-nerd-icons)))
