{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.ext.virtualization.docker.devdns;
in
{
  options = {
    ext.virtualization.docker.devdns = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker internal DNS service";
      };
      restartPolicy = mkOption {
        type = types.str;
        default = "unless-stopped";
        description = "DNS service restarting policy. See respective Docker docs for reference.";
      };
      port = mkOption {
        type = types.str;
        default = "5380";
        description = "Port to bind DNS service on.";
      };
      containerName = mkOption {
        type = types.str;
        default = "docker_dns";
        description = "Containet name for docker DNS service.";
      };
      autoStart = mkOption {
        default = false;
        type = types.bool;
        description = "Whether DNS service should be started automatically.";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.ext.virtualization.docker.core.enable;
        message = "virt/docker/devdns: enable docker/core first.";
      }];

      systemd.services."docker-devdns" = {
        description = "Dev DNS for docker containers";
        after = [ "docker.service" "docker.socket" ];
        requires = [ "docker.service" "docker.socket" ];
        script = ''
          ${pkgs.docker}/bin/docker run \
                                    --restart=${cfg.restartPolicy} \
                                    -p ${cfg.port}:${cfg.port} \
                                    --name=${cfg.containerName} \
                                    -v /var/run/docker.sock:/var/run/docker.sock \
                                    -v /etc/resolv.conf:/etc/resolv.conf \
                                    defreitas/dns-proxy-server
        '';
        preStop = "${pkgs.docker}/bin/docker stop ${cfg.containerName}";
        reload = "${pkgs.docker}/bin/docker restart ${cfg.containerName}";
        serviceConfig = {
          ExecStartPre = "-${pkgs.docker}/bin/docker rm -f ${cfg.containerName}";
          ExecStopPost = "-${pkgs.docker}/bin/docker rm -f ${cfg.containerName}";
          TimeoutStartSec = 0;
          TimeoutStopSec = 120;
          Restart = "always";
        };
      } // optionalAttrs (cfg.autoStart) {
        wantedBy = [ "multi-user.target" ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "Control" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl restart docker-devdns.service";
          mode = "dev";
        }
        {
          key = [ "Control" "Shift" "d" ];
          cmd = "${pkgs.systemd}/bin/systemctl stop docker-devdns.service";
          mode = "dev";
        }
      ];
    })
  ];
}
