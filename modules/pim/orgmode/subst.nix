{ config, inputs, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
in
{
  ditaaJar = "${pkgs.ditaa}/lib/ditaa.jar";
  emacsOrgRoamDotBinary = "${pkgs.graphviz}/bin/dot";
  emacsOrgRoamDir = homePrefix user "docs/org/roam";
  emacsOrgBarsPath = inputs.emacs-org-bars;
  emacsConsultOrgClockPath = inputs.emacs-consult-org-clock;
  orgRoot = config.pim.orgmode.rootDir;
  pimCommonCaptureDataTemplate = config.pim.orgmode.commonCaptureDataTemplate;
  pimOrgAgendaElPatch = config.pim.orgmode.agendaElPatch;
  xprintidleBinary = "${pkgs.xprintidle}/bin/xprintidle";
  orgRoamAutosyncEnable = if config.pim.orgmode.org-roam.autosync.enable then "(org-roam-db-autosync-enable)" else "";
  lspStartFunction = config.dev.misc.emacs.lsp.startFunction;
  lspModeGraphvizRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "lsp-mode"
    then emacsMkLspModeRegisterServer [ "graphviz-dot-mode-hook" ] [ "dot-language-server" "--stdio" ] "dot" "dotls"
    else "";
  eglotGraphvizRegisterServer =
    if config.dev.misc.emacs.lsp.impl == "eglot"
    then emacsMkEglotRegisterServer [ "graphviz-dot-mode-hook" ] [ "dot-language-server" "--stdio" ] [ "graphviz-dot-mode" ]
    else "";
}
