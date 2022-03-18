;;TODO: investigate major mode completions handling by examplke below:
;; (defun company-c-setup ()
;;   ;; make `company-backends' local is critical
;;   ;; or else, you will have completion in every major mode, that's very annoying!
;;   (make-local-variable 'company-backends)
;;   (setq company-backends (copy-tree company-backends)))
(use-package company
  :delight
  :preface
  (defun add-pcomplete-to-capf () ;; Enable company in Org mode
    (add-hook 'completion-at-point-functions #'pcomplete-completions-at-point nil t))
  (defun custom/company-complete-selection ()
    "Insert the selected candidate or the first if none are selected."
    (interactive)
    (if company-selection
        (company-complete-selection)
      (company-complete-number 1)))
  :bind
  ("C-<tab>" . company-complete)
  ("C-M-<tab>" . company-complete-common)
  (:map company-active-map
        ("<escape>" . company-abort)
        ("<tab>" . company-complete-selection)
        ("C-M-d" . company-quickhelp-manual-begin)
        ("C-M-s" . company-search-candidates)
        ("C-c ." . company-complete)
        ("C-c C-." . company-complete)
        ("C-d" . company-show-doc-buffer)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-s" . company-filter-candidates)
        ("M-." . company-show-location)
        ("M-<tab>" . custom/company-complete-selection)
        ("M-h" . company-show-doc-buffer)
        ("M-q" . company-other-backend)
        ("RET" . company-complete-selection))
  (:map company-search-map
        ("C-g" . company-search-abort)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("M-g" . company-search-toggle-filtering)
        ("M-j" . company-complete-selection)
        ("M-n" . company-search-repeat-forward)
        ("M-o" . company-search-kill-others)
        ("M-p" . company-search-repeat-backward)
        ("M-r" . company-search-repeat-backward)
        ("M-s" . company-search-repeat-forward))
  :hook
  (org-mode-hook . add-pcomplete-to-capf)
  (prog-mode-hook . company-mode)
  :custom-face
  (company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
  (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
  :custom
  (company-backends '(company-capf company-keywords company-elisp))
  (company-begin-commands '(self-insert-command
                            self-insert-command
                            org-self-insert-command
                            orgtbl-self-insert-command
                            c-scope-operator
                            c-electric-colon
                            c-electric-lt-gt
                            c-electric-slash ))
  (company-dabbrev-downcase nil)
  (company-echo-delay 0)
  (company-idle-delay 0)
  (company-lighter-base "")
  (company-minimum-prefix-length 1)
  (company-require-match 'never)
  (company-search-regexp-function #'company-search-flex-regexp) ;; originally #'regexp-quote
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-idle-delay 0.2)
  (company-tooltip-limit 20)
  (company-tooltip-minimum-width 80)
  (company-transformers '(company-sort-by-backend-importance
                          company-sort-prefer-same-case-prefix
                          company-sort-by-statistics
                          company-sort-by-occurrence))
  (completion-ignore-case t)
  :config
  (defadvice company-capf (around bar activate) ;; Ignore errors
    (ignore-errors add-do-it))
  (add-to-list 'company-frontends 'company-preview-frontend)
  (use-package company-dabbrev-code
    :custom
    (company-dabbrev-code-everywhere t)
    (company-dabbrev-code-other-buffers t)
    :config
    (add-to-list 'company-backends 'company-dabbrev-code))
  (use-package company-dabbrev
    :custom
    (company-dabbrev-char-regexp "[-_A-Za-z0-9]")
    (company-dabbrev-downcase nil)
    (company-dabbrev-ignore-case 'keep-prefix)
    (company-dabbrev-ignore-case nil)
    (company-dabbrev-other-buffers t)
    :config
    (add-to-list 'company-backends 'company-dabbrev))
  (use-package company-files
    :custom
    (company-files-chop-trailing-slash nil)
    :config
    (add-to-list 'company-backends 'company-files))
  (when (string-equal "i3" (getenv "CURRENT_WM"))
    (use-package company-quickhelp
      :custom
      (company-quickhelp-idle-delay 0.1)
      (company-quickhelp-delay nil)
      :config
      (company-quickhelp-mode 1)
      (use-package pos-tip
        :ensure t))
    (use-package company-statistics
      :config
      (company-statistics-mode))))

(use-package company-try-hard
  :after company
  :bind
  ("C-~" . company-try-hard))

(use-package company-box
  :after company
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (prog-mode-hook . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))
  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
            (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
            (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
            (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

(use-package company-restclient
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package company-prescient
  :after (company prescient)
  :custom
  (company-prescient-sort-length-enable nil)
  :config
  (company-prescient-mode +1))

;;; Prerequisite: Execute M-x company-tabnine-install-binary to install the TabNine binary for your system.
(use-package company-tabnine
  :after (company)
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
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
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
  (defadvice company-echo-show (around disable-tabnine-upgrade-message activate)
    (let ((company-message-func (ad-get-arg 0)))
      (when (and company-message-func
                 (stringp (funcall company-message-func)))
        (unless (string-match "The free version of TabNine only indexes up to" (funcall company-message-func))
          ad-do-it))))
  (add-to-list 'company-transformers 'company/sort-by-tabnine t)
  ;; (add-to-list 'company-backends #'company-tabnine)
  (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
  (company-tabnine-toggle t))

(use-package dabbrev
  :custom
  (dabbrev-case-distinction nil)
  (dabbrev-case-fold-search t)
  (dabbrev-case-replace nil))

(use-package yasnippet
  :delight yas-minor-mode
  :mode (("@emacsYasnippetSnippets@" . snippet-mode)
         ("\\.yasnippet$" . snippet-mode))
  :bind
  (:map custom-yasnippet-map
        ("i" . yas-insert-snippet))
  :config
  (yas-global-mode)
  :custom
  (yas-key-syntaxes '("w" "w_" "w_." "^ " "w_.()" yas-try-key-from-whitespace))
  (yas-expand-only-for-last-commands '(self-insert-command))
  (yas-prompt-functions '(yas-completing-prompt
                          yas-x-prompt
                          yas-no-prompt))
  (yas-wrap-around-region t)
  (yas-snippet-dirs '("@emacsYasnippetSnippets@")))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

(use-package text-mode
  :after company-dabbrev
  :hook (text-mode-hook . company-mode)
  :company (company-tabnine company-capf company-dabbrev))
