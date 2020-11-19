(use-package cc-mode
  :config
  (setq-default c-basic-offset 4))

;;TODO: play with https://github.com/MaskRay/ccls/wiki/lsp-mode#user-content-cross-reference-extensions later
(use-package ccls
  :preface
  (defun ccls/callee ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
  (defun ccls/caller ()
    (interactive)
    (lsp-ui-peek-find-custom "$ccls/call"))
  (defun ccls/vars (kind)
    (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels)
    (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind)
    (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))
  ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
  (defun ccls/references-address ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 128)))
  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 64)))
  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :excludeRole 32)))
  ;; References w/ Role::Read
  (defun ccls/references-read ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 8)))
  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (lsp-ui-peek-find-custom "textDocument/references"
                             (plist-put (lsp--text-document-position-params) :role 16)))
  :hook
  ((c-mode-hook c++-mode-hook) . (lambda () (require 'ccls) (lsp-deferred)))
  ((c-mode-hook c++-mode-hook) . ccls-code-lens-mode)
  :config
  (require 'ccls)
  :custom
  (ccls-executable "@cclsExecutable@")
  (ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (ccls-sem-highlight-method 'overlays)
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 32768))

(use-package cmake-mode)
(use-package cmake-font-lock)

;; Think of them as sp-{down,previous,next,down}-sexp for C/C++, roughly semantic movement among declarations.

;; (ccls-navigate "D") ;; roughly sp-down-sexp
;; (ccls-navigate "L")
;; (ccls-navigate "R")
;; (ccls-navigate "U")

;; Misc

;; For out-of-band changes to the files in the workspace that are not made in the LSP client (e.g. git pull), call (ccls-reload) to reload/rebuild indexes for every file.

;;     Performance of lsp-ui-flycheck https://github.com/emacs-lsp/lsp-ui/issues/45
