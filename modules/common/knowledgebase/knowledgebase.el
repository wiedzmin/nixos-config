(use-package apropos
  :bind
  (:map mode-specific-map
        :prefix-map custom-help-map
        :prefix "H"
        ("a" . apropos)
        ("d" . apropos-documentation)
        ("v" . apropos-variable)
        ("c" . apropos-command)
        ("l" . apropos-library)
        ("u" . apropos-user-option)
        ("i" . info-apropos)
        ("t" . tags-apropos)
        ("e" . apropos-value)))

(use-package helpful
  :defer t
  :bind
  (:map custom-help-map
        ("f" . helpful-function)
        ("v" . helpful-variable)
        ("C" . helpful-callable)
        ("M" . helpful-macro)
        ("c" . helpful-command)
        ("k" . helpful-key)
        ("RET" . helpful-at-point))
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
