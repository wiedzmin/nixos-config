(use-package files
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.c\\'" "\\1.h"))
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.h\\'" "\\1.c")))

(use-package cmake-mode)
(use-package cmake-font-lock)

(use-package modern-cpp-font-lock
  :delight
  :config
  (modern-c++-font-lock-global-mode t))

(use-package ob-C
  :commands (org-babel-expand-body:C org-babel-execute:C org-babel-expand-body:C++ org-babel-execute:C++))

(use-package ccls
  :preface
  (when (featurep 'lsp-ui)
    ;; NOTE: user content manipulation reference: https://github.com/MaskRay/ccls/wiki/lsp-mode#user-content-cross-reference-extensions
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
                               (plist-put (lsp--text-document-position-params) :role 16))))
  :hook
  ((c-mode-hook c++-mode-hook) . (lambda () (require 'ccls) (lsp-deferred)))
  ((c-mode-hook c++-mode-hook) . ccls-code-lens-mode)
  :config
  (require 'ccls)
  :custom
  (ccls-executable "@cclsExecutable@")
  (ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (ccls-sem-highlight-method 'overlays))
