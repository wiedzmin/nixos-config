(use-package nix-mode
  @rnixInitSection@
  :mode (("\\.nix$" . nix-mode)
         ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode))
  :hook
  (nix-mode-hook . (lambda () (setq-local tab-width 2)))
  @rnixHookSection@
  :company (company-tabnine company-capf)
  :capf #'pcomplete-completions-at-point)

(use-package company-nixos-options
  :disabled
  :config
  (add-to-list 'company-backends 'company-nixos-options))
