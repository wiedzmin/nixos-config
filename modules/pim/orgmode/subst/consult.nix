{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  emacsConsultOrgClockPath = inputs.emacs-consult-org-clock;
  orgRoot = config.pim.orgmode.rootDir;
  orgRoamRoot = config.pim.orgmode.org-roam.rootDir;
}
