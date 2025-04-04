;;TODO: investigate major mode completions handling by example below:
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
  ;NOTE: do we need `company-tooltip-mouse' here?
  (company-echo ((t (:inherit company-tooltip :background "firebrick4"))))
  (company-echo-common ((t (:inherit company-tooltip :foreground "cyan"))))
  (company-preview ((t (:inherit company-tooltip :foreground "wheat" :background "gray30" :weight bold :underline t))))
  (company-preview-common ((t (:inherit company-preview :background "gray30" :foreground "cyan"))))
  (company-preview-common--show-p ((t (:inherit company-preview :background "skyblue"))))
  (company-preview-search ((t (:inherit company-preview :background "white"))))
  (company-scrollbar-bg ((t (:inherit company-tooltip :background "#002b37"))))
  (company-scrollbar-fg ((t (:inherit company-tooltip :background "orange"))))
  (company-tooltip ((t (:foreground "#dfdfe1" :background "#393939" :weight bold :underline nil))))
  (company-tooltip-annotation ((t (:inherit company-tooltip :background "#002b37"))))
  (company-tooltip-annotation-selection ((t (:inherit company-tooltip :background "orange"))))
  (company-tooltip-common ((t (:inherit company-tooltip :foreground "black" :background "lightgrey" :weight bold :underline nil)))) ;TODO: play with '(:inherit font-lock-constant-face)
  (company-tooltip-common-selection ((t (:inherit company-tooltip-selection :background "gray80" :foreground "red" :weight bold :underline nil))))
  (company-tooltip-search ((t (:inherit company-tooltip :foreground "#002b37" :background "#244f36" :weight bold :underline nil))))
  (company-tooltip-selection ((t (:inherit company-tooltip :foreground "black" :background "steelblue" :weight bold :underline nil)))) ;TODO: play with '(:inherit font-lock-function-name-face)
  (lazy-highlight ((t (:inherit default :foreground "#CFD7E5"))))
  :custom
  (company-backends '(company-capf company-keywords company-files company-dabbrev company-dabbrev-code))
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.25)
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
  (define-advice company-capf
      (:around (orig-fun &rest args) company-capf-noerror)
    (ignore-errors (apply orig-fun args)))
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
  (use-package company-quickhelp
    :custom
    (company-quickhelp-idle-delay 0.1)
    (company-quickhelp-delay nil)
    :config
    (company-quickhelp-mode 1)
    (use-package pos-tip))
  (use-package company-statistics
    :config
    (company-statistics-mode)))

(use-package company-try-hard
  :after company
  :bind
  ("C-~" . company-try-hard))

(use-package company-box
  :after (company all-the-icons)
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode-hook . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
    ;; Prettify icons
    (define-advice company-box-icons--elisp
        (:override (candidate) custom/company-box-icons--elisp)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil))))))
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

(use-package text-mode
  :after company-dabbrev
  :hook (text-mode-hook . company-mode)
  :config
  (when (boundp 'company-backends)
    (add-to-list 'company-backends 'company-tabnine)
    (add-to-list 'company-backends 'company-capf)
    (add-to-list 'company-backends 'company-dabbrev)))
