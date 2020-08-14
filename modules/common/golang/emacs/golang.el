(use-package go-mode
  :no-require t
  :after (multi-compile)
  :mode ("\\.go$" . go-mode)
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp-deferred)
  (go-mode-hook . whitespace-turn-off)
  :bind
  (:map go-mode-map
        ("C-c C-c" . multi-compile-run)
        ("C-k" . sp-kill-hybrid-sexp)
        ("C-<down>" . sp-push-hybrid-sexp)
        ("C-<right>" . sp-slurp-hybrid-sexp))
  :company company-tabnine
  :config
  (use-package lsp-go
    :custom
    (lsp-clients-go-server-args
     '("--cache-style=always"
       "--diagnostics-style=onsave"
       "--format-style=goimports")))
  ;; (use-package dap-mode
  ;;   :quelpa
  ;;   (dap-mode :repo "emacs-lsp/dap-mode" :fetcher github))
  ;; (use-package dap-go)
  ;; references:
  ;; https://github.com/emacs-lsp/dap-mode#go-1
  ;; https://github.com/go-delve/delve/blob/master/Documentation/EditorIntegration.md
  ;; https://github.com/benma/go-dlv.el/
  ;; https://marketplace.visualstudio.com/_apis/public/gallery/publishers/ms-vscode/vsextensions/Go/0.13.1/vspackage
  ;; https://github.com/derekparker/delve/blob/master/Documentation/cli/getting_started.md
  ;; https://github.com/derekparker/delve/tree/master/Documentation
  ;; https://code.visualstudio.com/docs/editor/extension-gallery#_can-i-download-an-extension-directly-from-the-marketplace
  ;; https://stackoverflow.com/questions/37071388/how-can-i-install-visual-studio-code-extensions-offline/38866913#38866913
  (setq gofmt-command "goimports")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.usePlaceholders" t t)))
  (add-to-list 'multi-compile-alist
               '(go-mode . (("build/git" "go build -v ./..."
                             (locate-dominating-file buffer-file-name ".git"))
                            ("build/mod" "go build -v ./..."
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ("lint/git" "golangci-lint run ./..."
                             (locate-dominating-file buffer-file-name ".git"))
                            ("lint/git/fix" "golangci-lint run --fix ./..."
                             (locate-dominating-file buffer-file-name ".git"))
                            ("mod/deps/download" "go mod download"
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ("all/generate" "go generate -v ./..."
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ("bin/install" "go install -v ./cmd/..."
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ("docker/build" "docker-compose build"
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ("mod/deps/update" "go get -u ./..."
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ("mod/deps/gc" "go mod tidy"
                             (locate-dominating-file buffer-file-name "go.mod"))
                            ))))

(use-package flycheck-golangci-lint
  :after (flycheck go-mode)
  :hook (go-mode-hook . flycheck-golangci-lint-setup))

(use-package gotest
  :after (go-mode)
  :bind
  (:map go-mode-map
        ("C-c C-x f" . go-test-current-file)
        ("C-c C-x t" . go-test-current-test)
        ("C-c C-x p" . go-test-current-project)
        ("C-c C-x T" . go-test-current-benchmark)
        ("C-c C-x F" . go-test-current-file-benchmarks)
        ("C-c C-x P" . go-test-current-project-benchmarks)
        ("C-c C-x x" . go-run)))

(use-package go-tag
  :no-require t
  :after (go-mode)
  :bind
  (:map go-mode-map
        ("C-c t" . go-tag-add)
        ("C-c T" . go-tag-remove))
  :custom
  (go-tag-args '("-transform" "camelcase")))
