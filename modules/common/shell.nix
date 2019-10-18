{ config, lib, pkgs, ... }:
with lib;

let cfg = config.tools.shell;
in {
  options = {
    # TODO: refine options
    tools.shell = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable shell tools.";
      };
    };
  };

  config = mkIf cfg.enable {
    home-manager.users."${config.attributes.mainUser.name}" = {
      home.packages = [
        checkbashism
        seturgent
        shellcheck
        shell-hist
        tmsu                        # ?
        progress                    # use in automation
      ];
    };
  };
}
