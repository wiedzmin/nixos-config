(use-package corfu
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator))
  :hook
  ((text-mode-hook prog-mode-hook) . corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-count 14)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2)
  (corfu-min-width 40)
  (corfu-max-width 80)
  (corfu-separator ?\s)
  (corfu-scroll-margin 5)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current 'insert)
  :config
  (corfu-history-mode 1))

(use-package corfu-echo
  :after corfu
  :custom
  (corfu-echo-delay 0.25)
  :config
  (corfu-echo-mode 1))

(use-package corfu-info
  :after corfu)

(use-package corfu-indexed
  :after corfu
  :config
  (corfu-indexed-mode t))

(use-package corfu-popupinfo
  :after corfu
  :bind
  (:map corfu-map
        ("M-n" . corfu-popupinfo-scroll-down)
        ("M-p" . corfu-popupinfo-scroll-up)
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
        ("M-s M-s" . corfu-quick-complete)
        ("C-'" . corfu-quick-complete)
        ("C-q" . corfu-quick-insert)))

(use-package cape
  :after corfu
  :demand t
  :bind
  ("C-c p" . cape-prefix-map)
  :custom
  (cape-dabbrev-min-length 2)
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-abbrev)
  (add-hook 'completion-at-point-functions #'cape-dict)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; NOTE: Make these capfs composable
  (advice-add #'comint-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add #'pcomplete-completions-at-point :around #'cape-wrap-nonexclusive))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-default-style '(:padding -1 :stroke 0 :margin 0 :radius 0 :height 0.5 :scale 1))
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'corfu
    (setq lsp-completion-provider :none)))
