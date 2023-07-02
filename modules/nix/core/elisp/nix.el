(use-package nix-mode
  @nixLspInitSection@
  :mode (("\\.nix$" . nix-mode)
         ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode))
  :hook
  (nix-mode-hook . (lambda () (setq-local tab-width 2)))
  @nixLspHookSection@
  :config
  @nixLspNixdSection@
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'company-tabnine)
    (add-to-list 'company-backends 'company-capf))
  (add-to-list 'completion-at-point-functions #'pcomplete-completions-at-point))

(use-package company-nixos-options
  :disabled
  :config
  (add-to-list 'company-backends 'company-nixos-options))
