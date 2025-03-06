(use-package treesit
  :after lsp-mode ;; NOTE: without this, lsp-mode will fail to work with *-ts-* modes
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :preface
  (defun custom/treesit-install-all-grammars ()
    "Install all language grammars specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
        (treesit-install-language-grammar lang)
        (message "`%s' parser was installed." lang)
        (sit-for 0.75))))
  :commands (treesit-install-language-grammar custom/treesit-install-all-grammars)
  :bind
  (:map misc-editing-map
        ("T" . treesit-explore))
  :init
  @treesitLanguageSourceAlistPatch@
  @treesitModeRemapAlistPatch@
  :custom
  @treesitFontLockLevelPatch@
  ;; TODO: explore `treesit-extra-load-path' semantics
  (treesit-extra-load-path `(,(no-littering-expand-etc-file-name "treesit/grammars/")))
  :config
  (treesit-major-mode-setup))

(use-package treesit-auto
  :disabled ;; NOTE: lisp error "void-variable quote", maybe byte-code issue
  :custom
  (treesit-auto-install . 'prompt)
  (treesit-auto-fallback-alist
   '((toml-ts-mode . conf-toml-mode)
     (html-ts-mode . mhtml-mode)))
  :config
  ;; NOTE: There is customizable metadata, refer to `treesit-auto-recipe-list' for details
  ;; Install all languages when calling `treesit-auto-install-all'
  (setq treesit-language-source-alist (treesit-auto--build-treesit-source-alist))
  (treesit-auto-install-all)
  (global-treesit-auto-mode))

(use-package treesit-jump
  :load-path "@emacsTreesitJumpPath@"
  :bind
  (:map custom-goto-map
        ("." . treesit-jump-jump)
        ("!" . treesit-jump-delete)
        ("=" . treesit-jump-select)
        ("t" . treesit-jump-transient)
        ("^" . treesit-jump-parent-jump)))

(use-package treesit-fold
  :bind
  (:map mode-specific-map
        ("TAB" . treesit-fold-toggle))
  :config
  (global-treesit-fold-mode 1)
  (global-treesit-fold-indicators-mode 1)
  (treesit-fold-line-comment-mode 1))

(use-package combobulate
  :load-path "@emacsCombobulatePath@"
  :hook
  (prog-mode-hook . combobulate-mode))

(use-package treesit-util)
