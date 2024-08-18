(use-package consult-eglot
  :after (eglot consult embark)
  :bind
  (:map custom-goto-map
        ("s" . consult-eglot-symbols))
  :config
  (use-package consult-eglot-embark
    :config
    (consult-eglot-embark-mode)))
