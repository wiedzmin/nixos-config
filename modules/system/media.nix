{ config, lib, pkgs, ... }:
with lib;

let cfg = config.media;
in {
  options = {
    media = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable media support";
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
      ympd.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable YMPD";
      };
      opengl.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable OpenGL";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      users.users."${config.attributes.mainUser}".extraGroups = [ "audio" "video" ];
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
        pavucontrol
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
  ];
}
# TODO: check how ympd relates to user-level mpd service
