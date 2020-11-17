{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.dev.ccpp;
  user = config.attributes.mainUser.name;
in {
  options = {
    custom.dev.ccpp = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable C/C++ dev infrastructure.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Emacs C/C++ setup.";
      };
    };
  };

  config = mkMerge [
    (mkIf (cfg.enable) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ ccls clang clang-tools ];
      };
      environment.etc."security/limits.conf".text = ''
        * hard  nofile    32768
        * soft  nofile    32768
      '';
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.extraPackages = epkgs: [ epkgs.ccls epkgs.cmake-mode epkgs.cmake-font-lock ];
      ide.emacs.config = readSubstituted ../subst.nix ./emacs/ccpp.el;
    })
  ];
}
