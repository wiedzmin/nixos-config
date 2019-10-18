(use-package apropos
  :bind
  (:map mode-specific-map
        :prefix-map custom-help-map
        :prefix "H"
        ("a" . apropos)
        ("d" . apropos-documentation)
        ("v" . apropos-variable)
        ("c" . apropos-command)
        ("l" . apropos-library)
        ("u" . apropos-user-option)
        ("i" . info-apropos)
        ("t" . tags-apropos)
        ("e" . apropos-value)))

(use-package helpful
  :ensure t
  :defer t
  :bind
  (:prefix-map custom-help-map
               :prefix "<f1>"
               ("f" . helpful-function)
               ("v" . helpful-variable)
               ("C" . helpful-callable)
               ("m" . helpful-macro)
               ("c" . helpful-command)
               ("k" . helpful-key)
               ("RET" . helpful-at-point))
  (:map help-map
        ("f" . helpful-function)
        ("v" . helpful-variable)
        ("C" . helpful-callable)
        ("m" . helpful-macro)
        ("c" . helpful-command)
        ("k" . helpful-key)
        ("RET" . helpful-at-point)))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :config
  (which-key-mode))

(use-package which-key-posframe
  :ensure t
  :after which-key
  :hook
  (after-init-hook . which-key-posframe-mode)
  :custom
  (which-key-posframe-poshandler 'posframe-poshandler-frame-center))
