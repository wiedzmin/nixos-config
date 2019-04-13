
(use-package rst
  :mode ("\\.rst$" . rst-mode))

(use-package vimrc-mode
  :ensure t
  :mode ((".vim\\(rc\\)?$" . vimrc-mode)
         ("*pentadactyl*" . vimrc-mode)))

(use-package sh-script
  :mode (("bashrc$" . shell-script-mode)
         ("bash_profile$" . shell-script-mode)
         ("bash_aliases$" . shell-script-mode)
         ("bash_local$" . shell-script-mode)
         ("bash_completion$" . shell-script-mode)
         (".powenv$" . shell-script-mode)
         ("\\zsh\\'" . shell-script-mode))
  :config
  ;; zsh
  (setq system-uses-terminfo nil))

(use-package nginx-mode
  :ensure t
  :mode ("nginx" . nginx-mode))

(use-package csv-mode
  :ensure t
  :mode ("\\.csv" . csv-mode)
  :config
  (setq-default csv-align-padding 2))

(use-package jinja2-mode
  :ensure t
  :mode ("\\.j2$" . jinja2-mode))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.yaml\\'" . yaml-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode  ("\\Dockerfile" . dockerfile-mode))

(use-package docker-compose-mode
  :ensure t
  :mode ("docker-compose" . docker-compose-mode))

(use-package ini-mode
  :ensure t
  :mode ("\\.ini\\'" . ini-mode))

(use-package po-mode
  :ensure t
  :mode ("\\.po$\\|\\.po\\." . po-mode))

(use-package make-mode
  :mode ("[Mm]akefile" . makefile-mode))

;; TODO: (alex3rd) extend setup
(use-package format-all :ensure t)

(use-package gitignore-mode
  :ensure t
  :mode ("^.gitignore$" . gitignore-mode))

(use-package actionscript-mode
  :ensure t
  :mode ("\\.actionscript" . actionscript-mode))

(use-package json-mode
  :after (json-reformat json-snatcher)
  :mode ("\\.json$" . json-mode))

(use-package opascal
  :quelpa
  (opascal :repo "ki11men0w/emacs-delphi-mode" :fetcher github)
  :mode ("\\.\\(pas\\|dpr\\|dpk\\)\\'" . opascal-mode)
  :preface
  (defun custom/enclose-by-spaces (left right)
    "Insert symbols LEFT and RIGHT around a region or point."
    (interactive "r")
    (if (use-region-p) ; act on region
        (let ((start (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (insert " ")
            (goto-char start)
            (insert " ")))
      (progn ; act around point
        (insert " " " ")
        (backward-char 1))))
  :general
  (:keymaps 'mode-specific-map
            "a" 'custom/enclose-by-spaces)
  :custom
  (opascal-indent-level 2)
  :config
  (smartparens-mode -1))

(use-package sgml-mode
  :general
  (:keymaps 'html-mode-map
            "C-c C-w" 'html-wrap-in-tag))

(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown$" . markdown-mode)
         ("\\.md$" . markdown-mode)
         ("\\.mkd$" . markdown-mode)
         ("\\.pdc$" . markdown-mode)
         ("\\.README$" . markdown-mode))
  :general
  (:keymaps 'markdown-mode-map
            "C-c C-v" 'markdown-preview
            "C-<tab>" 'yas/expand))

(use-package graphql-mode
  :ensure t
  :mode ("\\.graphql$" . graphql-mode))


(use-package css-mode
  :mode ("\\.scss$" . css-mode))

(use-package css-eldoc
  :ensure t
  :after (eldoc)
  :hook (css-mode-hook . turn-on-css-eldoc))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode-hook . rainbow-mode))

(use-package company-restclient
  :ensure t
  :after (restclient company))

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" . plantuml-mode)
  :custom
  (plantuml-jar-path "/usr/share/plantuml/lib/plantuml.jar")
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))
