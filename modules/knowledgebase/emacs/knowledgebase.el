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
  :config
  (which-key-mode))
