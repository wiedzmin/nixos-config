(use-package nix-mode
  :mode (("\\.nix$" . nix-mode)
         ((rx (eval "configuration.nix") (zero-or-more anything) eol) . nix-mode))
  :hook
  (nix-mode-hook . (lambda () (setq-local tab-width 2))))

(use-package company-nixos-options
  :disabled
  :config
  (add-to-list 'company-backends 'company-nixos-options))
