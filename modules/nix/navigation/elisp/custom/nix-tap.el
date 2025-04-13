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
