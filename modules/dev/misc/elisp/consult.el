(use-package consult-lsp
  :bind
  (:map custom-goto-map
        ("s" . consult-lsp-symbols)
        ("f" . consult-lsp-file-symbols)
        ("d" . consult-lsp-diagnostics))
  (:map lsp-mode-map
        ([remap xref-find-apropos] . consult-lsp-symbols)))
