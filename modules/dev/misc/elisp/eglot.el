;; TODO: investigate eglot completions configuration
(use-package eglot
  :bind (:map eglot-mode-map
              ("C-c r"   . eglot-rename)
              ("C-c C-a" . eglot-code-actions)
              ("C-c C-f" . eglot-format)
              ("C-c C-F" . eglot-format-buffer)
              ("C-c C-i" . eglot-find-implementation))
  :custom
  (eglot-report-progress nil)
  (eglot-ignored-server-capabilities
   '(:documentLinkProvider :inlayHintProvider :documentOnTypeFormattingProvider))
  (eglot-autoshutdown t)
  (eglot-autoreconnect nil)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-idle-delay 0.0)
  (eldoc-echo-area-display-truncation-message nil)
  (eglot-extend-to-xref nil)
  (eglot-stay-out-of '(yasnippet tempel))
  (eglot-ignored-server-capabilities
   '(:hoverProvider
     :documentHighlightProvider
     :documentFormattingProvider
     :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider
     :colorProvider
     :foldingRangeProvider)))

(use-package eglot-tempel
  :after eglot)
