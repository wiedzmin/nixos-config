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
