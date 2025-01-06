(use-package ob-blockdiag
  :commands (org-babel-execute:blockdiag))

(use-package ob-ditaa
  :commands (org-babel-execute:ditaa
             org-babel-prep-session:ditaa))

(use-package ob-dot
  :commands (org-babel-execute:dot
             org-babel-expand-body:dot))

(use-package pikchr-mode
  :commands (org-babel-default-header-args:pikchr
             org-babel-execute:pikchr
             org-babel-prep-session:pikchr))

(use-package ob-plantuml
  :commands (org-babel-execute:plantuml
             org-babel-prep-session:plantuml
             org-babel-variable-assignments:plantuml))

(eval-after-load 'org
  (use-package org
    :custom
    (org-ditaa-jar-path "@ditaaJar@")))
