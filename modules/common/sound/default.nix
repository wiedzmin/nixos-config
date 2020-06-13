{ config, lib, pkgs, ... }:
with lib;

let cfg = config.custom.sound;
in {
  options = {
    custom.sound = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable media support";
      };
      volume.deltaFraction = mkOption {
        type = types.float;
        default = 0.1;
        description = "Sound volume delta fraction value";
      };
      volume.deltaPercents = mkOption {
        type = types.int;
        default = 10;
        description = "Sound volume delta percents value";
      };
      pulse.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Pulseaudio";
      };
      pulse.systemwide = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to run Pulseaudio system-wide";
      };
      pulse.daemonConfig = mkOption {
        type = types.attrs;
        default = { };
        description = "Pulseaudio daemon configuration";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable { users.users."${config.attributes.mainUser.name}".extraGroups = [ "audio" ]; })
    (mkIf cfg.pulse.enable {
      hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
        package = pkgs.pulseaudioFull; # 'full' for e.g. bluetooth
        systemWide = cfg.pulse.systemwide;
        daemon.config = cfg.pulse.daemonConfig;
      };
      environment.systemPackages = with pkgs; [ pasystray lxqt.pavucontrol-qt ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = "XF86AudioRaiseVolume";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players volume ${builtins.toString cfg.volume.deltaFraction}+";
          mode = "root";
        }
        {
          key = "XF86AudioLowerVolume";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players volume ${builtins.toString cfg.volume.deltaFraction}-";
          mode = "root";
        }
        {
          key = "XF86AudioPrev";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players previous";
          mode = "root";
        }
        {
          key = "XF86AudioPlay";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players play-pause";
          mode = "root";
        }
        {
          key = "XF86AudioNext";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players next";
          mode = "root";
        }
        {
          key = "XF86AudioMute";
          cmd = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
          mode = "root";
        }
        {
          key = "XF86AudioMicMute";
          cmd = "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
          mode = "root";
        }
        {
          key = "Alt+XF86AudioNext";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players position ${
              builtins.toString config.custom.content.players.deltaSeconds
            }+";
          mode = "root";
        }
        {
          key = "Alt+XF86AudioPrev";
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players position ${
              builtins.toString config.custom.content.players.deltaSeconds
            }-";
          mode = "root";
        }
        {
          key = "Alt+p";
          cmd = "${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt";
          mode = "root";
        }
      ];
    })
  ];
}
