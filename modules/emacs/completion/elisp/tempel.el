(use-package tempel
  :bind
  (:map misc-editing-map
        ("f" . tempel-complete)
        ("i" . tempel-insert))
  (:map tempel-map
        ("M-<right>" . tempel-next)
        ("M-<left>" . tempel-previous))
  :custom
  (tempel-path "@emacsTempelSnippetsPath@")
  (tempel-trigger-prefix "<")
  :config
  (global-tempel-abbrev-mode))

(use-package tempel-collection)
