(use-package consult-ghq
  :bind
  (:map custom-search-map
        ("C-f" . consult-ghq-find)
        ("C-g" . consult-ghq-grep))
  :custom
  (consult-ghq-find-function 'consult-find)
  (consult-ghq-grep-function 'consult-grep))
