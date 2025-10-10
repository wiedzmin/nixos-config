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
  (hkey-ace-window-setup)
  (hyperbole-mode 1))

(with-eval-after-load 'hyperbole
  (with-eval-after-load 'ace-window
    (push '(?l hkey-window-link "Hyperbole: Window Link") aw-dispatch-alist)
    (push '(?t hkey-throw   "Hyperbole: Throw") aw-dispatch-alist)
    (push '(?r hkey-replace "Hyperbole: Replace Here") aw-dispatch-alist)
    (push '(?i hkey-drag-item "Hyperbole: Drag Item") aw-dispatch-alist)))
