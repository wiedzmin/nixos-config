;; (defun eval-in-emacs (&rest s-exps)
;;   "Evaluate S-EXPS with emacsclient."
;;   (let ((s-exps-string (cl-strings:replace-all
;;                         (write-to-string
;;                          `(progn ,@s-exps) :case :downcase)
;;                         ;; Discard the package prefix.
;;                         "nyxt::" "")))
;;     (format *error-output* "Sending to Emacs:~%~a~%" s-exps-string)
;;     (uiop:run-program
;;      (list "emacsclient" "--eval" s-exps-string))))

(define-command-global org-capture ()
  "Run org-capture script."
  (format *error-output* "Running org-capture script")
  (let* ((buffer (current-buffer))
         (url (url buffer))
        (title (title buffer)))
    (uiop:run-program
     (list "org-capture" "-u" "'{url}'" "-t" "'{title}'" "-e" "title"))))


