(use-package auth-source
  :custom
  ;;TODO: investigate and setup ghub according to https://github.com/magit/ghub/blob/master/ghub.org#user-content-manually-creating-and-storing-a-token
  ;;TODO: check if it needed and resurrect .authinfo.gpg
  (auth-sources '("~/.authinfo.gpg")))

(use-package keychain-environment
  :ensure t
  :hook
  (after-init-hook . keychain-refresh-environment))

(use-package password-cache
  :custom
  (password-cache-expiry nil)
  (password-cache t))
