(define-command-global org-capture ()
  "Run org-capture script."
  (format *error-output* "Running org-capture script")
  (let* ((buffer (current-buffer))
         (url (url buffer))
        (title (title buffer)))
    (uiop:run-program
     (list "org-capture" "-u" "'{url}'" "-t" "'{title}'" "-e" "title"))))

;NOTE: broken
(define-command-global open-url-fallback ()
  "Opens current buffer url in `@fallbackBrowser@'."
  (let* ((buffer (current-buffer))
         (url (url buffer))
         (title (title buffer))
         (cmd-list (uiop:split-string "@fallbackBrowser@" :separator " "))
         (cmd (append cmd-list url)))
    (format *error-output* "Opening `%s' with `@fallbackBrowser@'" url)
    (uiop:run-program cmd)))
