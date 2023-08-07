(use-package avoid
  :custom
  (mouse-avoidance-mode 'jump))

(use-package transwin
  :after frame
  :bind
  (:map custom-frame-map
        ("v" . transwin-toggle)
        ("V" . transwin-ask)
        ("<up>" . transwin-inc)
        ("<down>" . transwin-dec)))

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
  :custom
  (hl-line-sticky-flag nil)
  (global-hl-line-sticky-flag nil)
  @currentLineHighlightFaceHlLinePatch@
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

(use-package menu-bar
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1))

(use-package faces
  :config
  (set-face-attribute 'mode-line-active nil :inherit 'mode-line)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line))
