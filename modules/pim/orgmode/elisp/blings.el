(use-package org-bullets
  :hook
  (org-mode-hook . org-bullets-mode))

(use-package org-bars
  :if (display-graphic-p)
  :load-path "@emacsOrgBarsPath@"
  :hook
  (org-mode-hook . org-bars-mode))
