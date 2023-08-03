(use-package corfu-history
  :config
  ;; NOTE: should be unconditionally depend on `savehist`?
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (corfu-history-mode 1))
