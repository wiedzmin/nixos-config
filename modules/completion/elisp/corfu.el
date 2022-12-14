(use-package corfu
  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)
  (corfu-count 14)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-min-width 40)
  (corfu-max-width 80)
  (corfu-separator ?\s)
  (corfu-scroll-margin 5)
  (corfu-quit-no-match 'separator)
  :config
  (global-corfu-mode 1))

(use-package corfu-echo
  :after corfu
  :custom
  (corfu-echo-delay 0.25)
  :config
  (corfu-echo-mode 1))

(use-package corfu-info
  :after corfu)

(use-package corfu-popupinfo
  :after corfu
  :bind
  (:map corfu-map
        ("M-n" . corfu-popupinfo-scroll-down)
        ("M-p" . corfu-popupinfo-scroll-up)
        ;TODO: should we use `corfu-popupinfo-map`?
        ("M-l" . corfu-popupinfo-location)
        ("M-d" . corfu-popupinfo-documentation)
        ("M-t" . corfu-popupinfo-toggle))
  :custom
  (corfu-popupinfo-min-width 40)
  (corfu-popupinfo-max-width 80)
  (corfu-popupinfo-delay corfu-auto-delay))

(use-package corfu-quick
  :after corfu
  :bind
  (:map corfu-map
        ("C-'" . corfu-quick-complete)
        ("C-q" . corfu-quick-insert)))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))
