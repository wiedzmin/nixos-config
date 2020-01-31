(use-package nix-mode
  :ensure t
  :mode (("\\.nix$" . nix-mode)
         ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode)))

(use-package company-nixos-options
  :ensure t
  :disabled
  :config
  (add-to-list 'company-backends 'company-nixos-options))
