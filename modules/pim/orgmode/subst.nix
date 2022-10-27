{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
in
rec {
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  emacsOrgRoamDotBinary = "${pkgs.graphviz}/bin/dot";
  emacsOrgRoamPath = homePrefix user "docs/org/roam";
  emacsOrgBarsPath = inputs.emacs-org-bars;
  orgRoot = config.pim.orgmode.rootDir;
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  xprintidleBinary = "${pkgs.xprintidle}/bin/xprintidle";
  orgRoamAutosyncEnable = if config.pim.orgmode.org-roam.autosync.enable then "(org-roam-db-autosync-enable)" else "";
}
