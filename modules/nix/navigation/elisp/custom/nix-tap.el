(require 'thingatpt)

(defun nixpkgs-py-thing-at-point ()
  "`thing-at-point' entity for Python packages references in Nixpkgs local repo."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#")
    (skip-chars-backward "[:alnum:]-:_/.#")
    (if (looking-at "npkg-py#\\([a-z0-9\-\:\/\.\_]+\\)")
        (cons (+ (point) 8) (match-end 0))
      nil)))

(put 'npkg-py 'bounds-of-thing-at-point
     'nixpkgs-py-thing-at-point)

(defun nixpkgs-py-search-thing-at-point ()
  "`thing-at-point' entity for searching among Python packages in Nixpkgs local repo."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#")
    (skip-chars-backward "[:alnum:]-:_/.#")
    (if (looking-at "npkg-py/\\([a-z0-9\-\:\/\.\_]+\\)")
        (cons (+ (point) 8) (match-end 0))
      nil)))

(put 'npkg-py-search 'bounds-of-thing-at-point
     'nixpkgs-py-search-thing-at-point)

(defun nixpkgs-all-thing-at-point ()
  "`thing-at-point' entity for all packages references in Nixpkgs local repo."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#")
    (skip-chars-backward "[:alnum:]-:_/.#")
    (if (looking-at "npkg#\\([a-z0-9\-\:\/\.\_]+\\)")
        (cons (+ (point) 5) (match-end 0))
      nil)))

(put 'npkg-all 'bounds-of-thing-at-point
     'nixpkgs-all-thing-at-point)

(defun nixpkgs-all-search-thing-at-point ()
  "`thing-at-point' entity for searching among all packages in Nixpkgs local repo."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#")
    (skip-chars-backward "[:alnum:]-:_/.#")
    (if (looking-at "npkg/\\([a-z0-9\-\:\/\.\_]+\\)")
        (cons (+ (point) 5) (match-end 0))
      nil)))

(put 'npkg-all-search 'bounds-of-thing-at-point
     'nixpkgs-all-search-thing-at-point)

(defun nixpkgs-nix-shell-thing-at-point ()
  "`thing-at-point' entity for opening terminal with requested packages provided by `nix shell'."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#>|")
    (skip-chars-backward "[:alnum:]-:_/.#>|")
    (if (looking-at "nsp>\\([a-z0-9\-\:\/\.\_\|]+\\)")
        (cons (+ (point) 4) (match-end 0))
      nil)))

(put 'npkg-nix-shell 'bounds-of-thing-at-point
     'nixpkgs-nix-shell-thing-at-point)

(defun nixpkgs-nix-shell-cwd-thing-at-point ()
  "`thing-at-point' entity for opening terminal with requested packages provided by `nix shell', using current directory."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#>|")
    (skip-chars-backward "[:alnum:]-:_/.#>|")
    (if (looking-at "nspc>\\([a-z0-9\-\:\/\.\_\|]+\\)")
        (cons (+ (point) 5) (match-end 0))
      nil)))

(put 'npkg-nix-shell-cwd 'bounds-of-thing-at-point
     'nixpkgs-nix-shell-cwd-thing-at-point)

(defun nixpkgs-nix-shell-repo-thing-at-point ()
  "`thing-at-point' entity for opening terminal with requested packages provided by `nix shell', using repo root."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#>|")
    (skip-chars-backward "[:alnum:]-:_/.#>|")
    (if (looking-at "nspr>\\([a-z0-9\-\:\/\.\_\|]+\\)")
        (cons (+ (point) 5) (match-end 0))
      nil)))

(put 'npkg-nix-shell-repo 'bounds-of-thing-at-point
     'nixpkgs-nix-shell-repo-thing-at-point)
