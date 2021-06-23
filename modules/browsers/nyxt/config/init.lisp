(in-package #:nyxt)

(load (nyxt-init-file "external.lisp"))
(load (nyxt-init-file "appearance.lisp"))

(define-configuration browser
    ((session-restore-prompt :always-restore)))

(define-configuration (buffer internal-buffer editor-buffer prompt-buffer)
    ((default-modes `(emacs-mode ,@%slot-default%))
     (download-engine :renderer)
     (conservative-word-move t)))

(define-configuration (buffer)
    ((password-interface (make-instance 'password:user-password-store-interface))))

(define-configuration (web-buffer)
    ((conservative-word-move t)
     (default-modes `(auto-mode
                      emacs-mode
                      certificate-exception-mode
                      reduce-tracking-mode
                      ,@%slot-default%))
     (override-map (define-key %slot-default% ; consider using 'base-mode
                       "M-," 'nyxt/web-mode:history-backwards
                       "M-." 'nyxt/web-mode:history-forwards
                       "C-g" 'reload-current-buffer
                       "C-x n" 'set-url-new-buffer
                       "C-c t" 'org-capture
                       "C-c v" 'nyxt/visual-mode:visual-mode
                       "C-c m" 'nyxt/message-mode:list-messages
                       "f2 n" 'make-window))))

;; turn off follow mode for buffers: it screws the access-time order
(define-configuration buffer-source
    ((prompter:follow-p nil)))

(define-configuration nyxt/web-mode:web-mode
    ((nyxt/web-mode:hints-alphabet "123456789")))

(define-configuration nyxt/auto-mode:auto-mode
    ((nyxt/auto-mode:prompt-on-mode-toggle t)))

