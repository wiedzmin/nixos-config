(use-package font-core
  :config
  (global-font-lock-mode 1))

(use-package font-lock
  :preface
  (defun custom/highlight-keywords ()
    ;; highlight additional keywords
    (font-lock-add-keywords nil '(("\\<\\(FIXME\\|FIX_ME\\|FIX ME\\):" 1 font-lock-warning-face t)))
    (font-lock-add-keywords nil '(("\\<\\(BUG\\|BUGS\\):" 1 font-lock-warning-face t)))
    (font-lock-add-keywords nil '(("\\<\\(TODO\\|TO DO\\NOTE\\|TBD\\):" 1 font-lock-warning-face t)))
    (font-lock-add-keywords nil '(("\\<\\(DONE\\|HACK\\):" 1 font-lock-doc-face t)))
    ;; highlight too long lines
    (font-lock-add-keywords nil '(("^[^\n]\\{120\\}\\(.*\\)$" 1 font-lock-warning-face t))))
  :hook ((emacs-lisp-mode-hook lisp-mode-hook python-mode-hook) . custom/highlight-keywords)
  :config
  (setq font-lock-maximum-decoration t))

(use-package face-remap
  :general
  ("C-=" 'text-scale-increase)
  ("C--" 'text-scale-decrease))

(use-package unicode-fonts
  :ensure t
  :after (persistent-soft)
  :config
  (unicode-fonts-setup))

(use-package doom-modeline
  :ensure t
  :custom
  (doom-modeline-height 25)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  :config
  (doom-modeline 1))

;; Also some other good-looking theme is "material-theme"
(use-package nord-theme :ensure t :config (load-theme 'nord t) :disabled)
(use-package kaolin-themes :ensure t :config (load-theme 'kaolin-dark t) :disabled)
(use-package hc-zenburn-theme :ensure t :config (load-theme 'hc-zenburn t))
(use-package darkburn-theme :ensure t :config (load-theme 'darkburn t) :disabled)
(use-package solarized-theme :ensure t :config (load-theme 'solarized-dark t) :disabled)

;; Providing dark enough colors, unless we are using an appropriate theme, Darkburn, for example
(when (boundp 'zenburn-colors-alist)
  (set-face-attribute 'default nil :background "#1A1A1A")
  (set-face-attribute 'region nil :background (cdr (assoc "zenburn-bg-2" zenburn-colors-alist))))

(use-package tooltip
  :config
  (tooltip-mode 0))

(use-package tool-bar
  :config
  (tool-bar-mode -1))

(use-package scroll-bar
  :config
  (scroll-bar-mode -1)
  (when (>= emacs-major-version 25)
    (horizontal-scroll-bar-mode -1)))

(use-package menu-bar
  :general
  (:keymaps 'mode-specific-map
            "b" 'toggle-debug-on-error
            "q" 'toggle-debug-on-quit)
  :config
  (menu-bar-mode -1))

(use-package hl-line
  :config
  (global-hl-line-mode 1))

(use-package time
  :config
  (display-time)
  :custom
  (display-time-day-and-date t)
  ;; (display-time-form-list (list 'time 'load))
  (display-time-world-list
   '(("Europe/Moscow" "Moscow")))
  (display-time-mail-file t)
  (display-time-default-load-average nil)
  (display-time-24hr-format t)
  (display-time-string-forms '( day " " monthname " (" dayname ") " 24-hours ":" minutes)))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward)
  (uniquify-separator ":")
  (uniquify-ignore-buffers-re "^\\*")
  (uniquify-strip-common-suffix nil))

(use-package avoid
  :config
  (mouse-avoidance-mode 'jump))
