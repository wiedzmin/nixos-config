(with-eval-after-load 'hyperbole
  (use-package nix-tap)

  (use-package nix-entities
    :config
    (defib custom/hypb/nix/list-python-packages ()
      "Find and list Python packages in `nixpkgs' local repo."
      (let ((package (thing-at-point 'npkg-py)))
        (when package
          (ibut:label-set package)
          (hact 'nix/list-python-packages package))))

    (defib custom/hypb/nix/search-python-packages ()
      "Find and list Python packages in `nixpkgs' local repo."
      (let ((package (thing-at-point 'npkg-py-search)))
        (when package
          (ibut:label-set package)
          (hact 'nix/search-python-packages package))))

    (defib custom/hypb/nix/search-all-packages ()
      "Find and list all packages in `nixpkgs' local repo."
      (let ((package (thing-at-point 'npkg-all)))
        (when package
          (ibut:label-set package)
          (hact 'nix/list-all-packages package))))))
