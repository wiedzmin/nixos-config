(use-package tempel
  :preface
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :bind
  (:map misc-editing-map
        ("f" . tempel-complete)
        ("i" . tempel-insert))
  (:map tempel-map
        ("M-<right>" . tempel-next)
        ("M-<left>" . tempel-previous))
  :hook
  (prog-mode-hook . tempel-setup-capf)
  (text-mode-hook . tempel-setup-capf)
  (prog-mode-hook . tempel-abbrev-mode)
  :custom
  (tempel-path "@emacsTempelSnippetsPath@")
  ;; (tempel-trigger-prefix "<")
  :config
  (global-tempel-abbrev-mode))

(use-package tempel-collection)
