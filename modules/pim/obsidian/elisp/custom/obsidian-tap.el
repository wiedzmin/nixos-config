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

(defun list-obsidian-vault-plugins-thing-at-point ()
  "`thing-at-point' entity for listing community plugins for specific vault."
  (save-excursion
    (skip-chars-forward "[:alnum:]-_>")
    (skip-chars-backward "[:alnum:]-_>")
    (if (looking-at "cplugs-->\\([a-z0-9\-]+\\)")
        (cons (+ (point) 9) (match-end 0))
      nil)))

(put 'obsidian-vault-plugins 'bounds-of-thing-at-point
     'list-obsidian-vault-plugins-thing-at-point)

(defun list-obsidian-vault-plugins-data-thing-at-point ()
  "`thing-at-point' entity for listing community plugins with custom settings for specific vault."
  (save-excursion
    (skip-chars-forward "[:alnum:]-_>")
    (skip-chars-backward "[:alnum:]-_>")
    (if (looking-at "cplugs-data-->\\([a-z0-9\-]+\\)")
        (cons (+ (point) 14) (match-end 0))
      nil)))

(put 'obsidian-vault-plugins-data 'bounds-of-thing-at-point
     'list-obsidian-vault-plugins-data-thing-at-point)
