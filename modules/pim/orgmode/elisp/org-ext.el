(use-package orgit
  :after org
  :bind
  (:map custom-org-map
        ("q" . orgit-store-link))
  :custom
  (orgit-store-reference t))

(use-package orglink
  :delight " *>"
  :bind
  (:map mode-specific-map
        ("l" . orglink-mode))
  :hook
  (emacs-lisp-mode-hook . orglink-mode)
  (nix-mode-hook . orglink-mode)
  (go-mode-hook . orglink-mode)
  :custom
  (orglink-match-anywhere t))
