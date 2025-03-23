{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

{
  orgRoamRoot = config.pim.orgmode.org-roam.rootDir;
  emacsOrgBarsPath = inputs.emacs-org-bars;
  emacsConsultOrgClockPath = inputs.emacs-consult-org-clock;
  orgRoot = config.pim.orgmode.rootDir;
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  xprintidleBinary = "${pkgs.xprintidle}/bin/xprintidle";
  orgRoamAutosyncEnable = if config.pim.orgmode.org-roam.autosync.enable then "(org-roam-db-autosync-enable)" else "";
}
