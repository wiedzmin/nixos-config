(require 'rg)

(defcustom nixpkgs-repo-root "~/workspace/repos/github.com/NixOS/nixpkgs/"
  "Nixpkgs local repository root."
  :type 'string)

(defun nix/list-python-packages (token)
  "Find and list Python packages in `nixpkgs' local repo."
  (find-dired-with-command
   (format "%s/pkgs/development/python-modules" nixpkgs-repo-root)
   (format "find . -type d \\( -name \"*%s*\" \\) -ls" token)))

(defun nix/search-python-packages (token)
  "Show search results for token in question among Python packages in `nixpkgs' local repo."
  (rg token "nix" (format "%s/pkgs/development/python-modules" nixpkgs-repo-root))
  (other-window 1))

(defun nix/list-all-packages (token)
  "Find and list all packages in `nixpkgs' local repo."
  (find-dired-with-command nixpkgs-repo-root (format "find . -type d \\( -name \"*%s*\" \\) -ls" token)))

(defun nix/search-all-packages (token)
  "Show search results for token in question among all packages in `nixpkgs' local repo."
  (rg token "nix" nixpkgs-repo-root)
  (other-window 1))
