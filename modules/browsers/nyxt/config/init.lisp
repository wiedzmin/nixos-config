(in-package #:nyxt-user)

(define-configuration (buffer web-buffer)
    ((default-modes (append '(emacs-mode) %slot-default%))
     (default-modes (append '(custom-mode) %slot-default%))
     (override-map (define-key %slot-default%
                       "M-," 'nyxt/web-mode:history-backwards
                       "M-." 'nyxt/web-mode:history-forwards
                       "C-g" 'reload-current-buffer
                       "C-x n" 'set-url-new-buffer))))

(define-configuration (buffer prompt-buffer)
    ((default-modes (append '(emacs-mode) %slot-default%))))

(define-configuration (buffer)
    ((default-modes `(emacs-mode ,@%slot-default%))
     ;; (download-engine :renderer)
     (password-interface (make-instance 'password:user-password-store-interface))))

(define-configuration prompt-buffer
    ((hide-single-source-header-p t)))

;; turn off follow mode for buffers: it screws the access-time order
(define-configuration buffer-source
  ((prompter:follow-p nil)))
