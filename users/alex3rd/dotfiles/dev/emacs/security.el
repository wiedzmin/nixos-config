(use-package auth-source
  :custom
  ;;TODO: investigate and setup ghub according to https://github.com/magit/ghub/blob/master/ghub.org#user-content-manually-creating-and-storing-a-token
  ;;TODO: check if it needed and resurrect .authinfo.gpg
  (auth-sources '("~/.authinfo.gpg")))

(use-package epa
  :after (epg)
  :config
  (epa-file-enable)
  :custom
  (epa-pinentry-mode 'loopback))

(use-package epg-config
  :after (epg)
  :custom
  (epg-gpg-program "gpg2")
  (epg-gpg-home-directory "~/.gnupg"))

(use-package password-cache
  :custom
  (password-cache-expiry nil)
  (password-cache t))

(use-package keychain-environment
  :ensure t
  :config
  (keychain-refresh-environment))
