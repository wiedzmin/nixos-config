(use-package files
  :config
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.c\\'" "\\1.h"))
  (add-to-list 'find-sibling-rules '("\\([^/]+\\)\\.h\\'" "\\1.c")))

(use-package cmake-mode)
(use-package cmake-font-lock)

(use-package modern-cpp-font-lock
  :delight
  :init (modern-c++-font-lock-global-mode t))

(use-package ob-C
  :commands (org-babel-expand-body:C org-babel-execute:C org-babel-expand-body:C++ org-babel-execute:C++))
