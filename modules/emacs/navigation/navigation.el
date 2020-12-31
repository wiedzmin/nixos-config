(use-package amx
  :bind
  ("M-x" . amx)
  :custom
  (amx-backend 'ivy)
  (amx-save-file "@emacsDatadir@/amx-items"))

(use-package beginend
  :if (string-equal "i3" (getenv "CURRENT_WM"))
  :config
  (delight '((beginend-global-mode nil "beginend")
             (beginend-prog-mode nil "beginend")
             (beginend-dired-mode nil "beginend")
             (beginend-compilation-mode nil "beginend")
             (beginend-rg-mode nil "beginend")
             (beginend-magit-status-mode nil "beginend")))
  (beginend-global-mode))

(use-package flycheck-projectile
  :after (projectile flycheck)
  :bind
  (:map mode-specific-map
        ("p" . flycheck-projectile-list-errors))
  (:map flycheck-mode-map
        ("C-c ! p" . flycheck-projectile-list-errors)))

(use-package ivy-flycheck
  :after (ivy flycheck)
  :quelpa
  (ivy-flycheck :repo "caisah/ivy-flycheck" :fetcher github)
  :bind
  (:map mode-specific-map
        ("y" . ivy-flycheck))
  (:map flycheck-mode-map
        ("C-c ! o" . ivy-flycheck)))

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning-of-code-or-line)
  ([remap move-end-of-line] . mwim-end-of-code-or-line))

(use-package rainbow-delimiters
  :hook
  (prog-mode-hook . rainbow-delimiters-mode))

(use-package recursive-narrow
  :bind
  (:map mode-specific-map
        :prefix-map custom-narrowing-map
        :prefix "n"
        ("r" . narrow-to-region)
        ("d" . narrow-to-defun)
        ("w" . widen)
        ("N" . recursive-narrow-or-widen-dwim)
        ("D" . recursive-widen-dwim)))

(use-package whitespace
  :hook
  ((prog-mode-hook text-mode-hook) . whitespace-turn-on)
  :bind
  (:map mode-specific-map
        :prefix-map custom-ws-map
        :prefix "x"
        ("w" . whitespace-mode)
        ("W" . global-whitespace-mode))
  :custom
  (whitespace-line-column 121)
  (whitespace-style '(indentation::space
                      space-after-tab
                      space-before-tab
                      trailing
                      lines-tail
                      tab-mark
                      face
                      tabs)))
