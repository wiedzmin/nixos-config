;;TODO: some harness either here or within shell to automate the burden of setting up new golang project's boilerplate

;;TODO: update existing dep
;; example command: go get -v -u "github.com/group/project/package"
;; find/create command for squeezing dep references (see example above) from source tree
;; then feed them up to ivy and make elisp to deal with selection
(use-package go-mode
  :no-require t
  :after (multi-compile)
  :mode ("\\.go$" . go-mode)
  :hook
  (before-save-hook . gofmt-before-save)
  (go-mode-hook . lsp)
  (go-mode-hook . whitespace-turn-off)
  :bind
  (:map go-mode-map
        ("C-c C-c" . multi-compile-run))
  :config
  (setq gofmt-command "goimports")
  (add-to-list 'multi-compile-alist
               '(go-mode . (("build/git" "go build -v ./..."
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

(use-package go-guru
  :hook (go-mode-hook . go-guru-hl-identifier-mode))

(use-package flycheck-gometalinter
  :after flycheck
  :custom
  ;; only run fast linters
  (flycheck-gometalinter-fast t)
  ;; use in tests files
  (flycheck-gometalinter-test t)
  (flycheck-gometalinter-deadline "10s")
  ;; gometalinter: skips 'vendor' directories and sets GO15VENDOREXPERIMENT=1
  (flycheck-gometalinter-vendor t)
  ;; gometalinter: only enable selected linters
  (flycheck-gometalinter-disable-all t)
  (flycheck-gometalinter-enable-linters
   '("golint" "vet" "errcheck"))
  :config
  (flycheck-gometalinter-setup))

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

(setenv "GOPRIVATE" "@golangPrivateModules@")
