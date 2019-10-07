(use-package company-restclient
  :ensure t
  :after company restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package jinja2-mode
  :ensure t
  :mode "\\.j2$")

(use-package yaml-mode
  :mode "\\.yml\\'"
  :quelpa
  (yaml-mode :repo "yoshiki/yaml-mode" :fetcher github :version original))

(use-package rainbow-mode
  :ensure t
  :hook (css-mode-hook . rainbow-mode))

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "@emacsPlantumlJarPath@")
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))
