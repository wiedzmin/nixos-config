{ config, lib, pkgs, ... }:
with lib;

let cfg = config.services.xsuspender;
in {
  options = {
    services.xsuspender = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable XSuspender.
        '';
      };

      debug = mkOption {
        description = "Whether to enable debug output.";
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services."xsuspender" = {
      description = "Xsuspender";
      after = [ "graphical-session-pre.target" ];
      partOf = [ "graphical-session.target" ];
      wantedBy = [ "graphical-session.target" ];
      environment = optionalAttrs cfg.debug { G_MESSAGE_DEBUG = "all"; };
      serviceConfig = {
        PIDFile = "/run/xsuspender.pid";
        Restart = "always";
        RestartSec = 1;
        ExecStartPre = "${config.systemd.package}/bin/systemctl --user import-environment DISPLAY XAUTHORITY";
        ExecStart = "${pkgs.xsuspender}/bin/xsuspender";
      };
      restartTriggers = [ "%h/.config/xsuspender.conf" ];
    };
  };
}
