(use-package detached
  :init
  (use-package detached-init
    :config
    (detached-init))
  :bind (;; Replace `async-shell-command' with `detached-shell-command'
         ([remap async-shell-command] . detached-shell-command)
         ;; Replace `compile' with `detached-compile'
         ([remap compile] . detached-compile)
         ([remap recompile] . detached-compile-recompile)
         ;; Replace built in completion of sessions with `consult'
         ([remap detached-open-session] . detached-consult-session))
  :custom ((detached-show-output-on-attach t)
           (detached-terminal-data-command system-type))
  :config
  (add-to-list 'display-buffer-alist (cons "\\*Detached Shell Command\\*" (cons #'display-buffer-no-window nil))))

(use-package flycheck-checkbashisms
  :hook (flycheck-mode-hook . flycheck-checkbashisms-setup))

(use-package pueue
  :bind
  (:map mode-specific-map
        ("q" . pueue)))

(with-eval-after-load 'sh-mode
  (with-eval-after-load 'company
    (when (boundp 'company-backends)
      (add-to-list 'company-backends 'company-tabnine)
      (add-to-list 'company-backends 'company-capf)))
  (add-to-list 'completion-at-point-functions #'pcomplete-completions-at-point))
