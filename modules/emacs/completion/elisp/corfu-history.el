(use-package corfu-history
  :config
  (corfu-history-mode 1))

(with-eval-after-load 'projectile
  (with-eval-after-load 'corfu
    (add-to-list 'savehist-additional-variables 'corfu-history)))
