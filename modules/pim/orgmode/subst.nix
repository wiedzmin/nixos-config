{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
in
{
  emacsOrgRoamDir = homePrefix user "docs/org/roam";
  emacsOrgBarsPath = inputs.emacs-org-bars;
  emacsConsultOrgClockPath = inputs.emacs-consult-org-clock;
  orgRoot = config.pim.orgmode.rootDir;
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  xprintidleBinary = "${pkgs.xprintidle}/bin/xprintidle";
  orgRoamAutosyncEnable = if config.pim.orgmode.org-roam.autosync.enable then "(org-roam-db-autosync-enable)" else "";
}
