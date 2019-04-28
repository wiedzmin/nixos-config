(use-package info
  :preface
  (defun custom/open-info (topic bname)
    "Open info on TOPIC in BNAME."
    (if (get-buffer bname)
        (progn
          (switch-to-buffer bname)
          (unless (string-match topic Info-current-file)
            (Info-goto-node (format "(%s)" topic))))
      (info topic bname)))
  (defun custom/info-org () (custom/open-info "org" "*org info*"))
  (defun custom/info-org-clocking () (org-info "Clocking commands"))
  (defun custom/info-elisp () (custom/open-info "elisp" "*elisp info*"))
  (defun custom/info-emacs () (custom/open-info "emacs" "*emacs info*"))
  (defun custom/info-gcl () (custom/open-info "gcl" "*hyperspec*"))
  :general
  (:keymaps 'mode-specific-map
            "C-h o" 'custom/open-org
            "C-h ?" 'custom/info-org-clocking
            "C-h e" 'custom/open-elisp
            "C-h m" 'custom/open-emacs
            "C-h h" 'custom/open-gcl)
  :config
  (info-initialize)
  (setq Info-additional-directory-list
        (list (at-homedir "help/info")))
  (add-to-list 'Info-directory-list "/usr/share/info")
  (add-to-list 'Info-directory-list
               (format "%selpa/%s"
                       user-emacs-directory
                       (car (directory-files (at-config-basedir "elpa") nil "^use-package-")))))

(use-package helpful
  :ensure t
  :defer t
  :general
  ;;TODO: investigate more concise way
  ;;TODO: use C-u version (catch up TAPs)
  (:prefix "<f1>"
           "f" 'helpful-function
           "v" 'helpful-variable
           "C" 'helpful-callable
           "m" 'helpful-macro
           "c" 'helpful-command
           "k" 'helpful-key
           "RET" 'helpful-at-point)
  (:prefix "C-h"
           "f" 'helpful-function
           "v" 'helpful-variable
           "C" 'helpful-callable
           "m" 'helpful-macro
           "c" 'helpful-command
           "k" 'helpful-key
           "RET" 'helpful-at-point))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

(use-package which-key-posframe
  :quelpa
  (which-key-posframe :repo "yanghaoxie/which-key-posframe" :fetcher github)
  :config
  (which-key-posframe-enable)
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-frame-center))

(use-package help-find-org-mode
  :ensure t
  :pin melpa-stable
  :config (help-find-org-mode t))

(use-package info-buffer
  :ensure t
  :general
  ("C-h i" 'info-buffer))

(use-package info-colors
  :ensure t
  :hook (Info-selection-hook . info-colors-fontify-node))

(use-package woman
  :config
  (defalias 'man 'woman) ;'Woman' offers completion better than 'man'.
  (setenv "MANPATH" "/usr/share/man:/usr/local/man"))

(use-package apropos
  :general
  (:keymaps 'mode-specific-map
            "H a" 'apropos
            "H d" 'apropos-documentation
            "H v" 'apropos-variable
            "H c" 'apropos-command
            "H l" 'apropos-library
            "H u" 'apropos-user-option
            "H i" 'info-apropos
            "H t" 'tags-apropos
            "H e" 'apropos-value))

(use-package info-look)

(setq custom/hyperspec-root "~/help/HyperSpec/")

;; lookup information in hyperspec
(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))
