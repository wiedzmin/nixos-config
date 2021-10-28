{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

let
  user = config.attributes.mainUser.name;
in
rec {
  deftPath = homePrefix user "docs/deft";
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  emacsOrgRoamDotBinary = "${pkgs.graphviz}/bin/dot";
  emacsOrgRoamPath = homePrefix user "docs/org/roam";
  orgRoot = config.pim.orgmode.rootDir;
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  xprintidleBinary = "${pkgs.xprintidle-ng}/bin/xprintidle-ng";
}
