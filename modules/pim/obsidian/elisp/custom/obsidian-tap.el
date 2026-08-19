(require 'thingatpt)

(defun search-obsidian-plugin-thing-at-point ()
  "`thing-at-point' entity for searching Obsidian plugins under all existing vaults."
  (save-excursion
    (skip-chars-forward "[:alnum:]-:_/.#>|")
    (skip-chars-backward "[:alnum:]-:_/.#>|")
    (if (looking-at "obsp#\\([a-z0-9\-\:\/\.\_\|]+\\)")
        (cons (+ (point) 5) (match-end 0))
      nil)))

(put 'obsidian-plugin 'bounds-of-thing-at-point
     'search-obsidian-plugin-thing-at-point)
