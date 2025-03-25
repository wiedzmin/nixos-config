(use-package eww
  :custom
  (eww-auto-rename-buffer 'title)
  (shr-cookie-policy same-origin)
  :bind
  (:map eww-mode-map
        ("M-o" . browse-url-at-point)
        ("y y" . eww-copy-page-url)
        ("^" . eww-up-url)))
