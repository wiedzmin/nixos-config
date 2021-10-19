(use-package company
  :delight
  :bind
  ("C-<tab>" . company-complete)
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)
        ("C-c ." . company-complete)
        ("C-c C-." . company-complete))
  :hook (prog-mode-hook . company-mode)
  :custom
  (company-echo-delay 0)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  (company-require-match 'never)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 20)
  (company-dabbrev-downcase nil)
  (company-selection-wrap-around t)
  :config
  (when (string-equal "i3" (getenv "CURRENT_WM"))
    (use-package company-quickhelp
      :custom
      (company-quickhelp-idle-delay 0.1)
      :config
      (company-quickhelp-mode 1)
      (use-package pos-tip
        :ensure t))
    (use-package company-statistics
      :config
      (company-statistics-mode))))

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

(use-package company-tabnine
  :after (company)
  :preface
  (defun company/sort-by-tabnine (candidates) ;; Integrate company-tabnine with lsp-mode
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
        (nconc (seq-take candidates-tabnine 3)
               (seq-take candidates-lsp 6)))))
  ;TODO: bind 'company-other-backend
  :custom
  (company-tabnine-max-num-results 10)
  :config
  (add-to-list 'company-transformers 'company/sort-by-tabnine t)
  (add-to-list 'company-backends '(company-capf :with company-tabnine :separate)))

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
  (yas-snippet-dirs '(yas-installed-snippets-dir
                      "@emacsYasnippetSnippets@")))

(use-package all-the-icons-completion
  :hook
  (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :config
  (all-the-icons-completion-mode))

(use-package text-mode
  :after company-dabbrev
  :hook (text-mode-hook . company-mode)
  :company (company-tabnine company-capf company-dabbrev))
