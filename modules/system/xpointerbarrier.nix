{ config, lib, pkgs, ... }:
with lib;

let cfg = config.services.xpointerbarrier;
in {
  options = {
    services.xpointerbarrier = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = ''
          Whether to enable xpointerbarrier.
        '';
      };
      top = mkOption {
        type = types.int;
        default = 25;
        description = ''
          Top margin size.
        '';
      };
      left = mkOption {
        type = types.int;
        default = 0;
        description = ''
          Left margin size.
        '';
      };
      right = mkOption {
        type = types.int;
        default = 0;
        description = ''
          Right margin size.
        '';
      };
      bottom = mkOption {
        type = types.int;
        default = 0;
        description = ''
          Bottom margin size.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.services."xpointerbarrier" = {
      description = "Create pointer barriers around each XRandR screen";
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        PassEnvironment = "DISPLAY";
        ExecStart = ''
          ${pkgs.xpointerbarrier}/bin/xpointerbarrier ${builtins.toString cfg.top} \
                                                      ${builtins.toString cfg.left} \
                                                      ${builtins.toString cfg.right} \
                                                      ${builtins.toString cfg.bottom}
        '';
      };
    };
  };
}
