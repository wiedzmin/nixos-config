(use-package avoid
  :custom
  (mouse-avoidance-mode 'jump))

(use-package transwin
  :after frame
  :bind
  (:map custom-frame-map
        ("v" . transwin-toggle-transparent-frame)
        ("V" . transwin-ask-set-transparency)
        ("<up>" . transwin-increment-frame-transparent)
        ("<down>" . transwin-decrement-frame-transparent)))

(use-package default-text-scale
  :bind
  (:map mode-specific-map
        ("=" . default-text-scale-increase)
        ("-" . default-text-scale-decrease))
  :custom
  (default-text-scale-amount 10)
  :config
  (default-text-scale-mode 1))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package fringe
  :hook
  (server-after-make-frame-hook . (lambda () (set-fringe-style '(3 . 0)))))

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

(use-package lin
  :custom
  (lin-face 'lin-green-override-fg)
  :config
  (lin-global-mode 1))

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(set-face-attribute 'mode-line-active nil :inherit 'mode-line)
(set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
