{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.backups;
  user = config.attributes.mainUser.name;
in
{
  options = {
    workstation.backups = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable backups infra";
      };
      rootDir = mkOption {
        type = types.str;
        default = homePrefix user ".backups";
        description = "Root directory for backups";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable backups infra for Emacs.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable { })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [
        epkgs.backup-each-save
      ];
      ide.emacs.core.customPackages = {
        "backups-misc" = { text = readSubstituted config inputs pkgs [ ./subst/misc.nix ] [ ./elisp/custom/misc.el ]; };
      };
      ide.emacs.core.config = readSubstituted config inputs pkgs [ ./subst/misc.nix ] [ ./elisp/backups.el ];
    })
    (mkIf (cfg.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        ".backups" = { local.path = cfg.rootDir; };
      };
    })
  ];
}
