(use-package hyperbole
  :demand t
  :bind
  (:map custom-org-map
        ("h g" . hyrolo-grep))
  :custom
  (hyrolo-highlight-face '(:background "SystemWindowText" :foreground "purple1" :underline t))
  (hsys-org-enable-smart-keys :buttons)
  :config
  (setq hbmap:dir-user "~/.hyperb")
  (remove-hook 'hyrolo-edit-hook #'hyrolo-set-date)
  (remove-hook 'hyrolo-add-hook #'hyrolo-set-date)
  (hyperbole-mode 1))
