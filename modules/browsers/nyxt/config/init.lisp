(define-configuration (buffer)
  ((default-modes `(emacs-mode ,@%slot-default%))
   ;; (download-engine :renderer)
   (password-interface (make-instance 'password:user-password-store-interface))))

(define-configuration web-buffer
  ((default-new-buffer-url "https://duckduckgo.com")))

;; prompt-buffer
(define-configuration prompt-buffer
  ((hide-single-source-header-p t)))

;; turn off follow mode for buffers: it screws the access-time order
(define-configuration buffer-source
  ((prompter:follow-p nil)))
