(use-package nix-mode
  :mode (("\\.nix$" . nix-mode)
         ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode)))

(use-package company-nixos-options
  :disabled
  :config
  (add-to-list 'company-backends 'company-nixos-options))
