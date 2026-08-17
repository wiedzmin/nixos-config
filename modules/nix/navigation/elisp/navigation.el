(with-eval-after-load 'hyperbole
  (use-package nix-tap)
  (require 'rg)

  (defcustom nixpkgs-repo-root "~/workspace/repos/github.com/NixOS/nixpkgs/"
  "Nixpkgs local repository root."
  :type 'string)

  (defun nix/list-python-packages (token)
    "Find and list Python packages in `nixpkgs' local repo."
    (find-dired-with-command
     (format "%s/pkgs/development/python-modules" nixpkgs-repo-root)
     (format "find . -type d \\( -name \"*%s*\" \\) -ls" token)))

  (defib custom/hypb/nix/list-python-packages ()
    "Find and list Python packages in `nixpkgs' local repo."
    (let ((package (thing-at-point 'npkg-py)))
      (when package
        (ibut:label-set package)
        (hact 'nix/list-python-packages package))))

  (defun nix/search-python-packages (token)
    "Show search results for token in question among Python packages in `nixpkgs' local repo."
    (rg token "nix" (format "%s/pkgs/development/python-modules" nixpkgs-repo-root))
    (other-window 1))

  (defib custom/hypb/nix/search-python-packages ()
    "Find and list Python packages in `nixpkgs' local repo."
    (let ((package (thing-at-point 'npkg-py-search)))
      (when package
        (ibut:label-set package)
        (hact 'nix/search-python-packages package))))

  (defun nix/list-all-packages (token)
    "Find and list all packages in `nixpkgs' local repo."
    (find-dired-with-command nixpkgs-repo-root (format "find . -type d \\( -name \"*%s*\" \\) -ls" token)))

  (defun nix/search-all-packages (token)
    "Show search results for token in question among all packages in `nixpkgs' local repo."
    (rg token "nix" nixpkgs-repo-root)
    (other-window 1))

  (defib custom/hypb/nix/search-all-packages ()
    "Find and list all packages in `nixpkgs' local repo."
    (let ((package (thing-at-point 'npkg-all)))
      (when package
        (ibut:label-set package)
        (hact 'nix/list-all-packages package))))

  (defun nix/open-vt-nix-shell (token &optional use-cwd repo-root)
    "Make `nix shell' command from delimited list of packages and open VT for it, with
optional working directory setting."
    (let* ((tokens (split-string token "/" t " "))
           (packages (split-string (car tokens) "|" t " "))
           (path-parts (cadr tokens))
           (path (cond (use-cwd default-directory)
                       (repo-root (custom/vcs-root-current))
                       (path-parts
                        (format "/%s"
                                (mapconcat 'identity
                                           (cdr (split-string test-nsp-val-path "/" t " ")) "/")))
                       (t (getenv "HOME"))))
           (clauses (mapconcat (lambda (s) (format "\"nixpkgs#%s\"" s)) packages " ")))
      (open-vt (format "nix shell %s" clauses) path)))

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
        (hact 'nix/open-vt-nix-shell packages nil t)))))
