(use-package webpaste
  :bind
  (:map custom-webpaste-map
        ("b" . webpaste-paste-buffer)
        ("r" . webpaste-paste-region))
  :custom
  (webpaste-provider-priority '("ix.io" "gist.github.com")))

(use-package jinja2-mode
  :mode "\\.j2$")

(use-package yaml-pro) ;FIXME: review modes/keybindings/etc.

;; FIXME: check/workaround [un]supported LSP cap(s), at least for `eglot'
;; FIXME: consider extracting `tree-sitter` setup
(use-package yaml-mode
  :mode
  ("\\.yaml\\'" . yaml-mode)
  ("\\.yml\\'" . yaml-mode)
  ("\\Taskfile\\'" . yaml-mode)
  :hook
  @lspStartFunctionYaml@
  @lspModeYamlRegisterServer@
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)
        ("<return>" . newline-and-indent)))

(use-package justl
  :defer t
  :custom
  (justl-executable "@justBinary@")
  (justl-recipe-width 25))

(use-package fic-mode
  :hook
  (prog-mode . fic-mode))

(use-package elmacro)

(use-package comby
  :custom
  (comby-args '("-exclude" "@combyExcludes@")))

(use-package bug-reference)

(defun open-project (path)
  (cond ((f-directory? (format "%s/%s" path ".git")) (magit-status path))
        ((f-directory? (format "%s/%s" path ".hg")) (vc-dir path))
        (t (dired path))))

; FIXME: unwire from `kitty'
(defun open-vt (cmd &optional path)
  (kill-new (format "%s" cmd))
  (let ((cwd (if path path "~")))
    (apply #'start-process "kitty" nil (list "kitty" "-1" "--listen-on" "tcp:localhost:12345" "--working-directory" cwd))))

(use-package 0x0
  :bind
  (:map custom-webpaste-map
        ("0" . 0x0-upload-text)))

(use-package hl-prog-extra
  :config
  (global-hl-prog-extra-mode))

(use-package jq-format
  :bind
  (:map mode-specific-map
        ;; NOTE: use "delete-indentation" on region to stringify the data back (beware of spaces separation afterwards)
        ("j" . jq-format-json-region)))
