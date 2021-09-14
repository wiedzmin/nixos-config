{ config, inputs, lib, pkgs, ... }:

rec {
  combyExcludes = lib.concatStringsSep "," config.dev.misc.comby.excludes;
  emacsConsultLsp = inputs.emacs-consult-lsp;
  plantumlJar = "${pkgs.plantuml}/lib/plantuml.jar";
}
