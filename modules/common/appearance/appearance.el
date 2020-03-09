(use-package avoid
  :custom
  (mouse-avoidance-mode 'jump))

(use-package doom-modeline
  :hook
  (after-init-hook . doom-modeline-init)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil))

(use-package default-text-scale
  :bind
  ("C-=" . default-text-scale-increase)
  ("C--" . default-text-scale-decrease)
  :custom
  (default-text-scale-amount 10)
  :config
  (default-text-scale-mode 1))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package prism
  :quelpa
  (prism :repo "alphapapa/prism.el" :fetcher github)
  :hook
  (prog-mode-hook . prism-mode)
  (python-mode-hook . prism-whitespace-mode)
  :bind
  (:map prog-mode-map
        ("C-c C-p" . prism-mode))
  (:map python-mode-map
        ("C-c C-w" . prism-whitespace-mode)))

(use-package time
  :config
  (display-time)
  :custom
  (display-time-day-and-date t)
  (display-time-form-list (list 'time 'load))
  (display-time-world-list
   '(("@systemTimeZone@" "@systemTimeZone@")))
  (display-time-mail-file t)
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-string-forms '( day " " monthname " (" dayname ") " 24-hours ":" minutes)))

(use-package unicode-fonts
  :after persistent-soft
  :hook
  (after-init-hook . unicode-fonts-setup))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator ":")
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-strip-common-suffix nil))

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode))

(use-package rainbow-mode
  :hook
  (css-mode-hook . rainbow-mode))
