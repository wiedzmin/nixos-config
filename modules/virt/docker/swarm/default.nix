{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.ext.virtualization.docker.swarm;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in {
  options = {
    ext.virtualization.docker.swarm = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Docker Swarm setup";
      };
      meta = mkOption {
        type = types.attrs;
        default = { };
        description = "Swarm --> entrypoint host mappings.";
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
      assertions = [
        {
          assertion = config.ext.virtualization.docker.core.enable;
          message = "virt/docker/devdns: enable docker/core first.";
        }
        {
          assertion = config.workstation.systemtraits.enable;
          message = "virt: must enable systemtraits maintainence.";
        }
        {
          assertion = config.ext.networking.vpn.enable;
          message = "virt: must enable vpn functionality.";
        }
      ];

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set virt/swarm_meta ${
          lib.strings.escapeNixString (builtins.toJSON cfg.meta)
        }
        ${pkgs.redis}/bin/redis-cli expire virt/swarm_meta 604800
      '';

      nixpkgs.config.packageOverrides = _: rec {
        docker_swarm_services_info = mkPythonScriptWithDeps "docker_swarm_services_info"
          (with pkgs; [ docker nurpkgs.pystdlib python3Packages.libtmux python3Packages.redis vpnctl yad ])
          (readSubstituted ../../../subst.nix ./scripts/docker_swarm_services_info.py);
      };

      environment.systemPackages = with pkgs; [ docker_swarm_services_info ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "i" ];
          cmd = "${pkgs.docker_swarm_services_info}/bin/docker_swarm_services_info";
          desktop = "shell";
          mode = "virt";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          docker_swarm_services_info
        ];
      };
    })
  ];
}
