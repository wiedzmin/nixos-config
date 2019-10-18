{ config, lib, pkgs, ... }:
with lib;

let cfg = config.tools.content;
in {
  options = {
    tools.content = {
      consumers.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-consumering tools.";
      };
      compression.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable (de)compression tools.";
      };
      imageTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools for working with images.";
      };
      videoTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable tools for working with videos.";
      };
      orderingTools.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable various ordering/deduplication tools.";
      };
      mpd.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MPD";
      };
      ympd.enable = mkOption { # TODO: check how ympd relates to user-level mpd service
        type = types.bool;
        default = false;
        description = "Whether to enable YMPD";
      };
      ympd.port = mkOption {
        type = types.str;
        default = "9090";
        description = "Port for YMPD to listen on";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.consumers.enable {
      # transmission + service https://transmissionbt.com/ + stig
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          android-file-transfer
          aria2
          gallery-dl
          jmtpfs # consider providing some (shell) automation
          qbittorrent
          uget
          wayback_machine_downloader
          you-get
          ytcc
        ];
      };
    })
    (mkIf cfg.compression.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          archiver
          pbzip2
          pigz
          unar
        ];
      };
    })
    (mkIf cfg.imageTools.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          exif
          exiftool
          exiv2
          gimp
          jhead
          mediainfo
          mediainfo-gui
          rawtherapee
        ];
      };
    })
    (mkIf cfg.videoTools.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          ( # FIXME: make closure for last working version
            mpv-with-scripts.override (
              {
                scripts = [ mpvScripts.mpris ];
              }
            )
          )
          ccextractor
          ffmpeg
          clipgrab
        ];
      };
    })
    (mkIf cfg.videoTools.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          dupd
          jdupes
          rmlint
          fpart
        ];
      };
    })
    (mkIf cfg.mpd.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        services.mpd = { # TODO: check options' values
          enable = true;
        };
      };
    })
    (mkIf cfg.ympd.enable {
      assertions = [
        {
          assertion = cfg.ympd.enable && cfg.mpd.enable;
          message = "content: enabling YMPD makes no sense without enabling MPD beforehand.";
        }
      ];

      services.ympd = {
        enable = true;
        webPort = cfg.ympd.port;
      };
    })
  ];
}
