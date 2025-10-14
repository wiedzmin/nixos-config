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
          (hact 'nix/list-all-packages package))))

    (defib custom/hypb/nix/nix-shell-terminal ()
      "Opens terminal window, allowing to paste `nix shell' clause with desired package list."
      (let ((packages (thing-at-point 'npkg-nix-shell)))
        (when packages
          (ibut:label-set packages)
          (hact 'nix/open-vt-nix-shell packages))))

    (defib custom/hypb/nix/nix-shell-cwd-terminal ()
      "Opens terminal window, allowing to paste `nix shell' clause with desired package list, using current directory."
      (let ((packages (thing-at-point 'npkg-nix-shell-cwd)))
        (when packages
          (ibut:label-set packages)
          (hact 'nix/open-vt-nix-shell packages t))))

    (defib custom/hypb/nix/nix-shell-repo-terminal ()
      "Opens terminal window, allowing to paste `nix shell' clause with desired package list, using repo root path."
      (let ((packages (thing-at-point 'npkg-nix-shell-repo)))
        (when packages
          (ibut:label-set packages)
          (hact 'nix/open-vt-nix-shell packages nil t))))))
