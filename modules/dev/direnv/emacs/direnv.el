(when @direnvGranularityProject@
  (use-package direnv
    :config
    (direnv-mode)))

(when @direnvGranularityFile@
  (use-package envrc
    :delight " ☀"
    :config
    (envrc-global-mode)))

(use-package nix-buffer)
