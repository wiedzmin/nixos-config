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
  (:prefix-map custom-pass-map
               :prefix  "<f6>"
               ("p" . pass)
               ("!" . ivy-pass))
  :config
  (use-package ivy-pass))
