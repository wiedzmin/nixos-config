{ config, lib, pkgs, ... }:
with lib;

let cfg = config.custom.dev.ccpp;
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
      home-manager.users."${config.attributes.mainUser.name}" = { home.packages = with pkgs; [ ccls clang ]; };
      environment.etc."security/limits.conf".text = ''
        * hard  nofile    32768
        * soft  nofile    32768
      '';
      custom.dev.git.gitignore = ''
        .ccls-root
      '';
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        programs.emacs.extraPackages = epkgs: [ epkgs.ccls ];
      };
      ide.emacs.config = builtins.readFile
        (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./ccpp.el; }));
    })
  ];
}
