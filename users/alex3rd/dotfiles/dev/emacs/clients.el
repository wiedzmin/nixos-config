(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-ssh-controlmaster-options "")
  (setq tramp-default-proxies-alist nil))

(use-package docker-tramp :ensure t)

(use-package vagrant-tramp :ensure t)

;;TODO: rebind to something
(use-package counsel-tramp
  :ensure t
  :after (docker-tramp vagrant-tramp))


(use-package httprepl :ensure t)

(use-package emamux
  :ensure t
  :general
  (:prefix "<f12>"
           "n" 'emamux:new-window
           "s" 'emamux:send-region
           "r" 'emamux:run-command))

(use-package w3m
  :ensure t
  :commands w3m
  :hook (w3m-display-hook . (lambda (url)
                              (rename-buffer
                               (format "*w3m: %s*" (or w3m-current-title
                                                       w3m-current-url)) t)))
  :custom
  (w3m-coding-system 'utf-8)
  (w3m-file-coding-system 'utf-8)
  (w3m-file-name-coding-system 'utf-8)
  (w3m-input-coding-system 'utf-8)
  (w3m-output-coding-system 'utf-8)
  (w3m-terminal-coding-system 'utf-8)
  (w3m-use-cookies t)
  :config
  ;; special chars
  (standard-display-ascii ?\200 [15])
  (standard-display-ascii ?\201 [21])
  (standard-display-ascii ?\202 [24])
  (standard-display-ascii ?\203 [13])
  (standard-display-ascii ?\204 [22])
  (standard-display-ascii ?\205 [25])
  (standard-display-ascii ?\206 [12])
  (standard-display-ascii ?\210 [23])
  (standard-display-ascii ?\211 [14])
  (standard-display-ascii ?\212 [18])
  (standard-display-ascii ?\214 [11])
  (standard-display-ascii ?\222 [?\'])
  (standard-display-ascii ?\223 [?\"])
  (standard-display-ascii ?\224 [?\"])
  (standard-display-ascii ?\227 " -- "))

(use-package w3m-search
  :after (w3m)
  :config
  (add-to-list 'w3m-search-engine-alist
               '("emacs-wiki" "http://www.emacswiki.org/cgi-bin/wiki.pl?search=%s")))

(use-package eww
  :preface
  (defun eww-more-readable () ;;TODO: add to appropriate hook
    "Makes eww more pleasant to use. Run it after eww buffer is loaded."
    (interactive)
    (setq eww-header-line-format nil) ;; removes page title
    (setq mode-line-format nil) ;; removes mode-line
    (set-window-margins (get-buffer-window) 20 20) ;; increases size of margins
    (redraw-display) ;; apply mode-line changes
    (eww-reload 'local))) ;; apply eww-header changes

(use-package footnote)

(use-package sendmail
  :custom
  (mail-specify-envelope-from t)
  (mail-envelope-from 'header)
  (send-mail-function 'sendmail-send-it))

(use-package message
  :hook (message-mode-hook . turn-on-orgtbl)
  :custom
  (message-sendmail-envelope-from 'header)
  (message-kill-buffer-on-exit t))

(use-package notmuch
  :ensure t
  :no-require t
  :commands notmuch
  :general
  (:keymaps 'notmuch-search-mode-map
            "!" '(lambda ()
                   "toggle unread tag for thread"
                   (interactive)
                   (if (member "unread" (notmuch-search-get-tags))
                       (notmuch-search-tag '("-unread" "-spam"))
                     (notmuch-search-tag '("+unread"))))
            "g" 'notmuch-refresh-this-buffer)
  (:keymaps 'notmuch-message-mode-map
            "#" 'mml-attach-file)
  (:keymaps 'mode-specific-map
            "4 n" 'notmuch
            "4 N" 'counsel-notmuch)
  :hook ((notmuch-hello-refresh-hook . (lambda ()
                                         (if (and (eq (point) (point-min))
                                                  (search-forward "Saved searches:" nil t))
                                             (progn
                                               (forward-line)
                                               (widget-forward 1))
                                           (if (eq (widget-type (widget-at)) 'editable-field)
                                               (beginning-of-line)))))
         (message-setup-hook . mml-secure-message-sign-pgpmime))  ;; Crypto Settings
  :custom
  (mm-text-html-renderer 'w3m)
  (notmuch-mua-compose-in 'current-window)
  (notmuch-identities private/gmail-accounts)
  (notmuch-fcc-dirs private/notmuch-fcc-dirs)
  (notmuch-search-line-faces '(("unread" . (:foreground "white"))
                               ("deleted" . (:foreground "red" :background "blue"))))
  (notmuch-saved-searches private/notmuch-saved-searches)
  (notmuch-crypto-process-mime t) ; Automatically check signatures
  (notmuch-hello-hide-tags (quote ("killed")))
  (notmuch-address-command "notmuch-addrlookup")
  :config
  (use-package org-notmuch
    :after (org notmuch))
  (use-package counsel-notmuch
    :ensure t
    :after (counsel notmuch)
    :commands counsel-notmuch))

;; try tagging from https://asynchronous.in/2017/04/21/Email-with-notmuch-and-astroid/

(use-package pdf-tools
  :ensure t
  :hook ((pdf-view-mode-hook . (pdf-links-minor-mode
                                pdf-outline-minor-mode))
         (after-init-hook . pdf-tools-install))
  :config
  (use-package pdf-view
    :ensure nil
    :mode ("\\.pdf$" . pdf-view-mode)
    ;; :magic ("%PDF" . pdf-view-mode)
    :preface
    (defun custom/scroll-other-window (&optional arg)
      (interactive "P")
      (awhen (ignore-errors (other-window-for-scrolling))
        (let* ((buffer (window-buffer it))
               (mode (with-current-buffer buffer major-mode)))
          (cond
           ((eq mode 'pdf-view-mode)
            (save-selected-window
              (select-window it)
              (with-current-buffer buffer
                (pdf-view-next-page (cond ((eq arg '-) -1)
                                          ((numberp arg) arg)
                                          (t 1))))))
           (t (scroll-other-window arg))))))
    :general
    ("C-M-v" 'custom/scroll-other-window)
    (:keymaps 'pdf-view-mode-map
              "C-s" 'isearch-forward
              "h" 'pdf-annot-add-highlight-markup-annotation
              "t" 'pdf-annot-add-text-annotation
              "y" 'pdf-view-kill-ring-save
              "D" 'pdf-annot-delete)
    :hook ((after-init-hook . pdf-tools-install)
           (pdf-view-mode-hook . pdf-isearch-minor-mode)
           ;; (pdf-tools-enabled-hook . pdf-view-midnight-minor-mode)
           (pdf-view-mode-hook . (lambda () (cua-mode -1)))) ;; turn off cua so copy works
    :custom
    (pdf-view-midnight-colors (quote ("white smoke" . "#002b36"))) ;; more brightness in midnight mode
    (pdf-view-resize-factor 1.1) ;; more fine-grained zooming
    (pdf-view-display-size 'fit-page))
  (use-package pdf-annot
    :ensure nil
    :general
    (:keymaps 'pdf-annot-edit-contents-minor-mode-map
              "<return>" 'pdf-annot-edit-contents-commit
              "<S-return>" 'newline)
    :custom
    (pdf-annot-activate-created-annotations t)
    :config
    (advice-add 'pdf-annot-edit-contents-commit :after 'save-buffer)))


(use-package pass
  :ensure t
  :general
  (:prefix "<f6>"
           "p" 'pass
           "!" 'ivy-pass)
  :config
  (use-package ivy-pass :ensure t))

(imagemagick-register-types)

(use-package wttrin
  :ensure t
  :after (xterm-color)
  :custom
  (wttrin-default-cities '("Moscow")))

(use-package webpaste
  :ensure t
  :general
  (:prefix "M-p"
           "b" 'webpaste-paste-buffer
           "r" 'webpaste-paste-region)
  :custom
  (webpaste-provider-priority '("ix.io" "gist.github.com")))

(use-package atomic-chrome
  :ensure t
  :custom
  (atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-server-ghost-text-port 4001)
  :config
  ;; TODO: (alex3rd) make use of atomic-chrome-url-major-mode-alist
  (atomic-chrome-start-server))

(use-package carbon-now-sh
  :quelpa
  (carbon-now-sh :repo "wiedzmin/carbon-now-sh.el" :fetcher github))

(use-package twittering-mode
  :ensure t
  :general
  (:keymaps 'mode-specific-map
            "5 t" 'twit)
  :init
  (setq twittering-use-master-password t)
  (setq twittering-private-info-file (expand-file-name "~/docs/enc/cred/.twittering-mode.gpg")))

(use-package telega
  :disabled
  :quelpa
  (telega :repo "zevlg/telega.el" :fetcher github :version original)
  :custom
  (telega-completing-read-function #'ivy-completing-read)
  :hook (telega-root-mode . telega-notifications-mode)
  :config
  (use-package telega-notifications))

(use-package edit-server
  :ensure t
  :config
  (edit-server-start))
