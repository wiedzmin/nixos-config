(use-package webpaste
  :bind
  (:map custom-webpaste-map
        ("b" . webpaste-paste-buffer)
        ("r" . webpaste-paste-region))
  :custom
  (webpaste-provider-priority '("ix.io" "gist.github.com")))

(use-package jinja2-mode
  :mode "\\.j2$")

(use-package yaml-mode
  :mode
  ("\\.yaml\\'" . yaml-mode)
  ("\\.yml\\'" . yaml-mode)
  ("\\Taskfile\\'" . yaml-mode)
  :bind
  (:map yaml-mode-map
        ("C-m" . newline-and-indent)
        ("<return>" . newline-and-indent)))

(use-package just-mode)

(use-package justl
  :custom
  (justl-executable "@justBinary@")
  (justl-recipe-width 25))

(use-package fic-mode
  :hook
  (prog-mode . fic-mode))

(use-package elmacro)

(use-package comby
  :custom
  (comby-args '("-exclude" "@combyExcludes@")))

(use-package plantuml-mode
  :mode "\\.plantuml\\'"
  :custom
  (plantuml-jar-path "@plantumlJar@")
  (org-plantuml-jar-path plantuml-jar-path)
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

(use-package blockdiag-mode)

(use-package bug-reference)

(use-package leetcode
  :disabled
  :custom
  (leetcode-prefer-language "python3")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory "~/workspace/leetcode"))

(use-package groovy-mode
  :custom
  (groovy-indent-offset 4)
  (groovy-highlight-assignments t))

(defun open-project (path)
  (cond ((f-directory? (format "%s/%s" path ".git")) (magit-status path))
        (t (dired path))))

(use-package 0x0
  :bind
  (:map custom-webpaste-map
        ("0" . 0x0-upload-text)))
