(use-package apropos
  :bind
  (:map help-map
        ("d" . apropos-documentation)
        ("c" . apropos-command)
        ("l" . apropos-library)
        ("u" . apropos-user-option)
        ("i" . info-apropos)
        ("t" . tags-apropos)
        ("e" . apropos-value)))

(use-package helpful
  :disabled
  :bind
  (:map help-map
        ("f" . helpful-function)
        ("v" . helpful-variable)
        ("C" . helpful-callable)
        ("M" . helpful-macro)
        ("c" . helpful-command)
        ("k" . helpful-key)
        ("RET" . helpful-at-point)))

(use-package which-key
  :delight
  :bind
  (:map help-map
        ("g" . which-key-show-top-level)
        ("j" . which-key-show-major-mode)
        ("C-u" . which-key-undo))
  :custom
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (which-key-idle-delay 10000)
  (which-key-idle-secondary-delay 0.05)
  (which-key-show-early-on-C-h t)
  (which-key-sort-order 'which-key-key-order-alpha)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode))
