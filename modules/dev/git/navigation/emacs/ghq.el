(use-package consult-ghq
  :bind
  (:map custom-nav-map
        ("C-f" . consult-ghq-find)
        ("C-g" . consult-ghq-grep)))
