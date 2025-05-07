{ config, pkgs, ... }:

{
  orgRoot = config.pim.orgmode.rootDir;
  xprintidleBinary = "${pkgs.xprintidle}/bin/xprintidle";
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
}
