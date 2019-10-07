(use-package avoid
  :custom
  (mouse-avoidance-mode 'jump))

(use-package doom-modeline
  :ensure t
  :hook
  (after-init-hook . doom-modeline-init)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil))

(use-package default-text-scale
  :ensure t
  :bind
  ("C-=" . default-text-scale-increase)
  ("C--" . default-text-scale-decrease)
  :custom
  (default-text-scale-amount 10)
  :config
  (default-text-scale-mode 1))

(use-package zenburn-theme
  :ensure t
  :hook
  (after-init-hook . (lambda () (load-theme 'zenburn t))))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package time
  :config
  (display-time)
  :custom
  (display-time-day-and-date t)
  (display-time-form-list (list 'time 'load))
  (display-time-world-list
   '(("@timeZone@" "@timeZone@")))
  (display-time-mail-file t)
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-string-forms '( day " " monthname " (" dayname ") " 24-hours ":" minutes)))

(use-package unicode-fonts
  :ensure t
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
  :ensure t
  :hook
  (dired-mode . diredfl-mode))
