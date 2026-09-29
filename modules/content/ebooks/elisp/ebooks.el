(use-package nov
  :mode
  ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :mode
  ("\\.pdf\\'" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("/" . pdf-occur)
        (">" . pdf-view-goto-page)
        ("G" . pdf-view-last-page)
        ("g" . pdf-view-first-page)
        ("h" . image-backward-hscroll)
        ("i" . pdf-misc-display-metadata)
        ("j" . pdf-view-next-page)
        ("k" . pdf-view-previous-page)
        ("l" . image-forward-hscroll)
        ("u" . pdf-view-revert-buffer)
        ("y" . pdf-view-kill-ring-save))
  (:map pdf-annot-minor-mode-map
        ("a" . pdf-annot-attachment-dired)
        ("d" . pdf-annot-delete)
        ("l" . pdf-annot-list-annotations)
        ("m" . pdf-annot-add-markup-annotation)
        ("t" . pdf-annot-add-text-annotation))
  :hook
  (pdf-view-mode-hook . pdf-view-midnight-minor-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-loader-install)
  (pdf-annot-minor-mode 1))
