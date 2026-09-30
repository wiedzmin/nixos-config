{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# nsp>usbview|lsb-release
# nsp>usbview npkg#usbview
# nsp>lsb-release npkg#lsb-release

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
          Whether to enable system traits maintenance.

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
        home.packages = with pkgs; [ nurpkgs.redis-tui bluemail ] ++ config.attributes.transientPackages;
        programs.television = {
          settings.shell_integration.channel_triggers = {
            "redis-keys" = [
              "redis-cli"
            ];
          };
          channels = {
            redis-keys = {
              metadata = {
                name = "redis-keys";
                description = "A channel to select keys from Redis";
                requirements = [ "redis" "jq" ];
              };
              source = {
                command = "redis-cli keys '*'";
                output = "{split: :1|trim:\"}";
              };
              preview = {
                command = "redis-cli get {split: :1|trim:\"}";
              };
            };
          };
        };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      completion.expansions.espanso.matches = {
        systemtraits = {
          matches = [
            {
              trigger = ":pms";
              replace = "sudo pmap -d $|$ | sort -k2 -n";
            }
            {
              trigger = ":pss";
              replace = "ps -o pid,user,%mem,command ax | sort -b -k3 -r";
            }
            {
              trigger = ":rcg";
              replace = "redis-cli $|$";
            }
          ];
        };
      };
    })
  ];
}
