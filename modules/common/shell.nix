{ config, lib, pkgs, ... }:
with lib;

let cfg = config.custom.shell;
in {
  options = {
    # TODO: refine options
    custom.shell = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell tools.";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell-related Emacs infra.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          checkbashism
          seturgent
          shellcheck
          shell-hist
          tmsu                        # ?
          progress                    # use in automation
        ];
      };
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nodePackages.bash-language-server
        ];
      };
    })
  ];
}
