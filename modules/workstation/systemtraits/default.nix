{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.workstation.systemtraits;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
  options = {
    workstation.systemtraits = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable system traits maintainence.

          Essential for custom scripts, etc.
        '';
      };
      instructions = mkOption {
        type = types.lines;
        default = "";
        description = ''
          Set of commands needed to initialize system traits cache.

          Currently, Redis is used.
        '';
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      services.redis.servers.default = {
        # NOTE: either explicitly set bind/port or use -s argument in redis-cli invocations
        enable = true;
        bind = "127.0.0.1";
        port = 6379;
      };
      systemd.services.redis-default.postStart = cfg.instructions;

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ nurpkgs.redis-tui usbview lsb-release ];
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/systemtraits.yml".text = ''
          name: systemtraits
          parent: default

          matches:
            - trigger: ":pms"
              replace: "sudo pmap -d $|$ | sort -k2 -n"

            - trigger: ":pss"
              replace: "ps -o pid,user,%mem,command ax | sort -b -k3 -r"

            - trigger: ":redkj"
              replace: "redis-cli keys '*' | cut -d\\  -f2 | fzf | xargs redis-cli get | jq ."

            - trigger: ":redkr"
              replace: "redis-cli keys '*' | cut -d\\  -f2 | fzf | xargs redis-cli get"
        '';
      };
    })
  ];
}
