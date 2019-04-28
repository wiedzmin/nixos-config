(require 'cl)
(require 'package)

(unless package--initialized
  (package-initialize))
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setf (cdr (assoc "gnu" package-archives))
      "https://elpa.gnu.org/packages/")
(setq package-archive-priorities
      '(("melpa-stable" . 5)
        ("gnu" . 5)
        ("melpa" . 10)))

(mapcar
 (lambda (package)
   (unless (package-installed-p package)
     (unless package-archive-contents
       (package-refresh-contents))
     (package-install package)))
 ;;base system packages for bootstrapping
 '(use-package el-get))

(setq use-package-compute-statistics t)
(setq use-package-verbose t)
(setq use-package-hook-name-suffix "")
(put 'use-package 'lisp-indent-function 1)

(use-package quelpa :ensure t :defer t)
(use-package quelpa-use-package
  :ensure t
  :custom
  (quelpa-use-package-inhibit-loading-quelpa
   t "Improve startup performance"))

(use-package use-package-secrets
  :custom
  (use-package-secrets-default-directory "/home/alex3rd/.emacs.d/secrets")
  :quelpa
  (use-package-secrets :repo "a13/use-package-secrets" :fetcher github :version original))

(use-package general :ensure t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(global-set-key (kbd "C-x C-.") ;; Run use-package linter
                (lambda ()
                  (interactive)
                  (find-file (concat user-emacs-directory "init.el"))
                  (use-package-lint)))
(global-set-key (kbd "C-x C-,") #'goto-char)

(use-package use-package-el-get
  :ensure t
  :defer t
  :config
  (use-package-el-get-setup))
