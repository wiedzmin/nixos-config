(use-package auth-source
  :config
  (add-to-list 'auth-sources "~/.authinfo"))

(use-package auth-source-pass
  :demand t
  :after auth-source
  :config
  (auth-source-pass-enable))

(use-package password-cache
  :custom
  (password-cache-expiry nil)
  (password-cache t))

(use-package pass
  :bind
  (:map custom-pass-map
        ("p" . pass))
  :custom
  (pass-show-keybindings t)
  (pass-username-field "login"))

(use-package password-store-menu
  :load-path "@emacsPasswordStoreMenuPath@"
  :bind
  (:map custom-pass-map
        ("!" . password-store-menu)))
