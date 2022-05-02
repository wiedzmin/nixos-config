{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
in
rec {
  deftPath = homePrefix user "docs/deft";
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  emacsOrgRoamDotBinary = "${pkgs.graphviz}/bin/dot";
  emacsOrgRoamPath = homePrefix user "docs/org/roam";
  emacsOrgBarsPath = inputs.emacs-org-bars;
  orgRoot = config.pim.orgmode.rootDir;
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  xprintidleBinary = "${pkgs.xprintidle}/bin/xprintidle";
}
