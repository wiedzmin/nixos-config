(require 'thingatpt)

(defun ghq-thing-at-point ()
  "`thing-at-point' entity for ghq-based repository paths."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#")
    (skip-chars-backward "[:alnum:]-:_/.#")
    (if (looking-at "ghq#\\([a-z0-9\-\~\:\/\.\_]+\\)")
        (cons (+ (point) 4) (match-end 0))
      nil)))

(put 'ghq-path 'bounds-of-thing-at-point
     'ghq-thing-at-point)

(defun ghq-search-thing-at-point ()
  "`thing-at-point' entity for searching ghq-based repos."
  (save-excursion
    (skip-chars-forward "[:alnum:]-_>")
    (skip-chars-backward "[:alnum:]-_>")
    (if (looking-at "ghq-search-->\\([a-z0-9\-\_]+\\)")
        (cons (+ (point) 13) (match-end 0))
      nil)))

(put 'ghq-search 'bounds-of-thing-at-point
     'ghq-search-thing-at-point)
