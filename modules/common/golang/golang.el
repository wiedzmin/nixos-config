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
  :config
  (use-package lsp-go)
  (setq gofmt-command "goimports")
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
                            ("mod/deps/update" "go get -v ./..."
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
