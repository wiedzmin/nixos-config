{ config, lib, pkgs, ... }:
with lib;

let
  cfg = config.workstation.sound;
  user = config.attributes.mainUser.name;
  prefix = config.wmCommon.prefix;
in {
  options = {
    # TODO: rename options in implementation-agnostic way
    workstation.sound = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Pulseaudio";
      };
      systemwide = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to run Pulseaudio system-wide";
      };
      daemonConfig = mkOption {
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
    (mkIf cfg.enable {
      users.users.${user}.extraGroups = [ "audio" ];

      hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
        package = pkgs.pulseaudioFull; # 'full' for e.g. bluetooth
        systemWide = cfg.systemwide;
        daemon.config = cfg.daemonConfig;
        extraConfig = ''
          load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
        '';
      };
      environment.systemPackages = with pkgs; [ pasystray lxqt.pavucontrol-qt ];
      # home-manager.users."${user}" = {
      #   # NOTE: temporary workaround
      #   home.activation.ensureSystemwidePulseaudio = {
      #     after = [ "checkLinkTargets" ];
      #     before = [ ];
      #     # FIXME: parameterize DBUS_SESSION_BUS_ADDRESS value
      #     data = ''
      #       export DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus
      #       ${pkgs.systemd}/bin/systemctl --user stop pulseaudio.service
      #       ${pkgs.systemd}/bin/systemctl restart pulseaudio.service
      #     '';
      #   };
      # };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "XF86AudioMute" ];
          cmd = "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";
          mode = "root";
        }
        {
          key = [ "XF86AudioMicMute" ];
          cmd = "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle";
          mode = "root";
        }
        {
          key = [ prefix "p" ];
          cmd = "${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt";
          mode = "root";
        }
      ];
    })
  ];
}
