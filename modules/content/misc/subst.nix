{ config, ... }:

{
  emacsEtcDir = config.ide.emacs.core.etcDir;
  emacsVarDir = config.ide.emacs.core.varDir;
  orgRoamRoot = config.pim.orgmode.org-roam.rootDir;
}
