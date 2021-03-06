(use-package atomic-chrome
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-server-ghost-text-port 4001)
  (atomic-chrome-url-major-mode-alist
   '(("reddit\\.com" . markdown-mode)
     ("github\\.com" . gfm-mode)
     ("redmine" . textile-mode))
   "Major modes for URLs.")
  :config
  (atomic-chrome-start-server))

(use-package browse-url
  :if (and (eq system-type 'gnu/linux)
           (eq window-system 'x))
  :custom
  (browse-url-browser-function 'browse-url-generic)
  (browse-url-generic-program "@emacsBrowserGenericProgram@"))

(use-package bruh
  :load-path "@emacsBruhPath@"
  :after browse-url
  :custom
  (browse-url-browser-function #'bruh-browse-url))
