{ config, ... }:

{
  orgRoamRoot = config.pim.orgmode.org-roam.rootDir;
  orgRoamAutosyncEnable = if config.pim.orgmode.org-roam.autosync.enable then "(org-roam-db-autosync-enable)" else "";
}
