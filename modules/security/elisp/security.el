(use-package auth-source
  :config
  (add-to-list 'auth-sources "~/.authinfo"))

(use-package password-cache
  :custom
  (password-cache-expiry nil)
  (password-cache t))
