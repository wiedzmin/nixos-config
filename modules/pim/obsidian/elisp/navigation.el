(with-eval-after-load 'hyperbole
  (use-package obsidian-tap)
  (require 'rg)

  (defun obsidian/search-community-plugins (token)
    "Find and list all vaults for which plugin is installed."
    (find-dired-with-command "@obsidianVaultsRoot@" (format "find . -type d \\( -name \"*%s*\" \\) -ls" token)))

  (defib custom/hypb/obsidian/search-community-plugins ()
    "Finds and lists all vaults for which plugin is installed."
    (let ((plugin (thing-at-point 'obsidian-plugin)))
      (when plugin
        (ibut:label-set plugin)
        (hact 'obsidian/search-community-plugins plugin)))))
