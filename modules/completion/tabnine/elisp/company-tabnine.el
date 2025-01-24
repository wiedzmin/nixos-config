;;; Prerequisite: Execute M-x company-tabnine-install-binary to install the TabNine binary for your system.
(use-package company-tabnine
  :disabled
  :after (company)
  :delight " ⌬"
  :preface
  (defun company/sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (progn
              (push candidate candidates-lsp)
              (puthash candidate t candidates-table))))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-lsp 2)
               (seq-take candidates-tabnine 2)
               (seq-drop candidates-lsp 2)
               (seq-drop candidates-tabnine 2)))))
  (defun company-tabnine-toggle (&optional enable)
    "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
    (interactive)
    (if (or enable (not (memq 'company-tabnine company-backends)))
        (progn
          (add-to-list 'company-backends #'company-tabnine)
          (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
          (add-to-list 'company-transformers 'company/sort-by-tabnine t)
          (message "TabNine enabled."))
      (setq company-backends (delete 'company-tabnine company-backends))
      (setq company-backends (delete '(company-capf :with company-tabnine :separate) company-backends))
      (setq company-transformers (delete 'company/sort-by-tabnine company-transformers))
      (company-tabnine-kill-process)
      (message "TabNine disabled.")))
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :custom
  (company-tabnine-max-num-results 10)
  (company-tabnine-max-restart-count 3)
  :config
  (define-advice company-echo-show
      (:around (orig-fun &rest args) disable-tabnine-upgrade-message)
    (let* ((company-message-func (car args))
           (result (funcall company-message-func)))
      (when (and company-message-func
                 (stringp result))
        (unless (string-match "The free version of TabNine only indexes up to" result)
          (apply orig-fun args)))))
  @tabnineExecutablePathPatch@
  (add-to-list 'company-transformers 'company/sort-by-tabnine t)
  ;; (add-to-list 'company-backends #'company-tabnine)
  (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
  (company-tabnine-toggle t))
