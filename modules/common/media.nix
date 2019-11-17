{ config, lib, pkgs, ... }:
with lib;

let cfg = config.custom.media;
in {
  options = {
    custom.media = {
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
        default = {};
        description = "Pulseaudio daemon configuration";
      };
      opengl.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable OpenGL";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users."${config.attributes.mainUser.name}".extraGroups = [ "audio" "video" ];
    })
    (mkIf (cfg.enable && config.attributes.staging.enable) {
      hardware.brillo.enable = true;
    })
    (mkIf cfg.pulse.enable {
      hardware.pulseaudio = {
        enable = true;
        support32Bit = true;
        package = pkgs.pulseaudioFull; # 'full' for e.g. bluetooth
        systemWide = cfg.pulse.systemwide;
        daemon.config = cfg.pulse.daemonConfig;
      };
      environment.systemPackages = with pkgs; [
        pasystray
        lxqt.pavucontrol-qt
      ];
    })
    (mkIf cfg.opengl.enable {
      hardware.opengl = {
        enable = true;
        extraPackages = with pkgs; [ intel-media-driver libvdpau-va-gl vaapiIntel vaapiVdpau ];
        driSupport32Bit = true;
        extraPackages32 = with pkgs.pkgsi686Linux; [ libvdpau-va-gl vaapiIntel vaapiVdpau ];
      };
      environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "<XF86AudioRaiseVolume>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players volume ${builtins.toString config.custom.media.volume.deltaFraction}+"'';
        "<XF86AudioLowerVolume>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players volume ${builtins.toString config.custom.media.volume.deltaFraction}-"'';
        "<XF86AudioPrev>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players previous"'';
        "<XF86AudioPlay>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players play-pause"'';
        "<XF86AudioNext>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players next"'';
        "<XF86AudioMute>" = ''spawn "${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle"'';
        "<XF86AudioMicMute>" = ''spawn "${pkgs.pulseaudio}/bin/pactl set-source-mute @DEFAULT_SOURCE@ toggle"'';
        "M-<XF86AudioNext>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players position ${builtins.toString config.custom.content.players.deltaSeconds}+"'';
        "M-<XF86AudioPrev>" = ''spawn "${pkgs.playerctl}/bin/playerctl --all-players position ${builtins.toString config.custom.content.players.deltaSeconds}-"'';
        "M-p" = ''spawn "${pkgs.lxqt.pavucontrol-qt}/bin/pavucontrol-qt"'';
      };
    })
  ];
}
