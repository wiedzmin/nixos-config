{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.content.media;
  user = config.attributes.mainUser.name;
  prefix = config.wmCommon.prefix;
in {
  options = {
    content.media = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-related tools.";
      };
      playback.deltaSeconds = mkOption {
        type = types.int;
        default = 10;
        description = "Player rewinding delta in seconds";
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
      mpris.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to MPRIS support.";
      };
      mpd.port = mkOption {
        type = types.int;
        default = 6600;
        description = "Port number MPD listens on.";
      };
      mpd.clients.enable = mkOption {
        type = types.bool;
        default = false;
        description = "whether to enable clients for MPD (and Mopidy as well)";
      };
      mpd.clients.ympd.port = mkOption {
        type = types.int;
        default = 8090;
        description = "Port number for YMPD interface";
      };
      mopidy.file.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Mopidy local media files management.";
      };
      mopidy.file.roots = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Media roots to search files under.";
      };
      mopidy.file.excludes = mkOption {
        type = types.listOf types.str;
        default = [ "html" "jpeg" "jpg" "log" "nfo" "pdf" "png" "txt" "zip" ];
        description = "Media files extensions to ignore.";
      };
      mopidy.youtube.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Mopidy setup for Youtube.";
      };
      mopidy.youtube.apiKey = mkOption {
        type = types.str;
        default = "";
        description = "Youtube API key.";
      };
      mopidy.youtube.maxEntries = mkOption {
        type = types.int;
        default = 300;
        description = "Maximum Youtube playlist entries count.";
      };
      youtubeFrontends.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable non-web Youtube frontends";
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
          assertion = (cfg.mopidy.youtube.enable && cfg.mopidy.youtube.apiKey != "");
          message = "mopidy: Youtube plugin enabled but no API key provided.";
        }
        {
          assertion = (cfg.mopidy.file.enable && cfg.mopidy.file.roots != { });
          message = "mopidy: Youtube plugin enabled but no API key provided.";
        }
      ];

      services.mopidy = {
        enable = true;
        extensionPackages = with pkgs;
          [ mopidy-local mopidy-mpd mopidy-youtube ] ++ lib.optionals (cfg.mpris.enable) [ mopidy-mpris ];
        configuration = ''
          [mpd]
          hostname = 0.0.0.0
          port = ${builtins.toString cfg.mpd.port}
          [audio]
          output = pulsesink server=127.0.0.1
        '' + lib.optionalString (cfg.mpris.enable) ''
          [mpris]
          enabled = true
        '' + lib.optionalString (cfg.mopidy.youtube.enable) ''
          [youtube]
          enabled = true
          api_enabled = true
          autoplay_enabled = true
          playlist_max_videos = ${builtins.toString cfg.mopidy.youtube.maxEntries}
          threads_max = 16
          search_results = 15
          youtube_api_key = ${cfg.mopidy.youtube.apiKey}
        '' + lib.optionalString (cfg.mopidy.file.enable) ''
          [file]
          enabled = true
          media_dirs =
          ${mkIndent 4}${
            lib.concatStringsSep "${mkNewlineAndIndent 4}"
            (lib.mapAttrsToList (tag: path: "${path}|${tag}") cfg.mopidy.file.roots)
          }
          show_dotfiles = false
          follow_symlinks = false
          metadata_timeout = 1000
        '' + lib.optionalString (cfg.mopidy.file.excludes != [ ]) ''
          excluded_file_extensions =
          ${mkIndent 4}${
            lib.concatStringsSep "${mkNewlineAndIndent 4}" (lib.forEach cfg.mopidy.file.excludes (ext: ".${ext}"))
          }
        '';
      };
      systemd.services.mopidy = {
        after = [ "network-online.target" ];
        path = [ pkgs.dbus ];
        serviceConfig = {
          User = lib.mkForce user;
          Group = "users";
          Environment = [ "DISPLAY=:0" ];
        };
      };
      environment.systemPackages = with pkgs; [ ncmpcpp ];
      home-manager.users.${user} = {
        programs.mpv = { # TODO: consider extracting options
          enable = true;
          scripts = with pkgs.mpvScripts;
            ([ sponsorblock thumbnail ] ++ # do we need custom [thumbnails] setup yet?
              lib.optionals (cfg.mpris.enable) [ mpris ]);
          config = {
            save-position-on-quit = true;
            hdr-compute-peak = false; # prevents brightness changes
            keep-open = true;
            watch-later-directory = "~/.local/share/mpv/watch_later";
            hwdec = "vdpau";
            hwdec-codecs = "all";
            vo = "gpu,xv";
            ao = "pulse";
            af = "scaletempo";
            audio-samplerate = "48000";
            slang = "en";
            alang = "en,eng,us";
            volume-max = "200";
            cache = "yes";
            cache-on-disk = "yes";
            cache-pause-initial = "yes";
            cache-pause-wait = "10";
            # # Always use 1080p+ or 60 fps where available. Prefer VP9
            # # over AVC and VP8 for high-resolution streams.
            # ytdl=yes
            # ytdl-format=(bestvideo[ext=webm]/bestvideo[height>720]/bestvideo[fps=60])[tbr<13000]+(bestaudio[acodec=opus]/bestaudio[ext=webm]/bestaudio)/best
          };
        };
        programs.zsh.shellAliases = { yg = "${pkgs.you-get}/bin/you-get"; };
      };
    })
    (mkIf (cfg.enable && cfg.mpd.clients.enable) {
      services.ympd = {
        enable = true;
        webPort = builtins.toString cfg.mpd.clients.ympd.port;
      };
      home-manager.users.${user} = { home.packages = with pkgs; [ ario sonata cantata ]; };
    })
    (mkIf (cfg.enable && cfg.youtubeFrontends.enable) {
      # TODO: try https://github.com/trizen/youtube-viewer
      home-manager.users.${user} = {
        # NOTE: default quotas seems inappropriate to use them freely
        home.packages = with pkgs; [ mps-youtube minitube smtube youtube-dl ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.wsMapping.rules = [
        {
          class = mkWSMappingBrowsersRegexp;
          title = "http youtube";
          scratchpad = true;
          key = [ "y" ];
        }
        {
          class = "cantata";
          scratchpad = true;
          key = [ "c" ];
        }
        {
          class = "Sonata";
          scratchpad = true;
          key = [ "n" ];
        }
      ];
      wmCommon.keys = [
        {
          key = [ "XF86AudioRaiseVolume" ];
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players volume ${builtins.toString cfg.volume.deltaFraction}+";
          mode = "root";
        }
        {
          key = [ "XF86AudioLowerVolume" ];
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players volume ${builtins.toString cfg.volume.deltaFraction}-";
          mode = "root";
        }
        {
          key = [ "XF86AudioPrev" ];
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players previous";
          mode = "root";
        }
        {
          key = [ "XF86AudioPlay" ];
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players play-pause";
          mode = "root";
        }
        {
          key = [ "XF86AudioNext" ];
          cmd = "${pkgs.playerctl}/bin/playerctl --all-players next";
          mode = "root";
        }
        {
          key = [ "Alt" "XF86AudioNext" ];
          cmd =
            "${pkgs.playerctl}/bin/playerctl --all-players position ${builtins.toString cfg.playback.deltaSeconds}+";
          mode = "root";
        }
        {
          key = [ "Alt" "XF86AudioPrev" ];
          cmd =
            "${pkgs.playerctl}/bin/playerctl --all-players position ${builtins.toString cfg.playback.deltaSeconds}-";
          mode = "root";
        }
      ];
    })
  ];
}
