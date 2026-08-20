(with-eval-after-load 'hyperbole
  (use-package obsidian-tap)

  (defun obsidian/search-community-plugins (token)
    "Find and list all vaults for which plugin is installed."
    (find-dired-with-command "@obsidianVaultsRoot@" (format "find . -type d \\( -name \"*%s*\" \\) -ls" token)))

  (defib custom/hypb/obsidian/search-community-plugins ()
    "Finds and lists all vaults for which plugin is installed."
    (let ((plugin (thing-at-point 'obsidian-plugin)))
      (when plugin
        (ibut:label-set plugin)
        (hact 'obsidian/search-community-plugins plugin))))

  (defun obsidian/list-community-plugins (vault)
    "Find and list all community plugins for specific vault."
    (find-dired-with-command
     (format "@obsidianVaultsRoot@/%s/.obsidian/plugins/" vault)
     "find . -maxdepth 1 -ls"))

  (defib custom/hypb/obsidian/search-community-plugins ()
    "Finds and lists all vaults for which plugin is installed."
    (let ((vault (thing-at-point 'obsidian-vault-plugins)))
      (when vault
        (ibut:label-set vault)
        (hact 'obsidian/list-community-plugins vault))))

  (defun obsidian/list-community-plugins-data (vault)
    "Find and list all community plugins for specific vault, with custom settings."
    (find-dired-with-command
     (format "@obsidianVaultsRoot@/%s/.obsidian/plugins" vault)
     "find . -name \"data.json\" -ls"))

  (defib custom/hypb/obsidian/search-community-plugins-data ()
    "Finds and lists all vaults for which plugin is installed."
    (let ((vault (thing-at-point 'obsidian-vault-plugins-data)))
      (when vault
        (ibut:label-set vault)
        (hact 'obsidian/list-community-plugins-data vault)))))
