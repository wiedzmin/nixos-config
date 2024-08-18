(use-package c-mode
  :mode (("\\.c$" . c-mode)
         ("\\.h$" . c-mode))
  ;; :after dap
  :config
  ;; (use-package dap-cpptools)
  (setq-default c-basic-offset 2))

(use-package c++-mode
  :mode (("\\.cpp$" . c++-mode)
         ("\\.h$" . c++-mode)
         ("\\.hpp$" . c++-mode))
  :config
  (setq-default c-basic-offset 2))

