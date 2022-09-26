{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.media;
  user = config.attributes.mainUser.name;
  inherit (config.wmCommon) prefix prefixAlt;
in
{
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
        description = "Whether to enable MPRIS support";
      };
      mpd.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MPD";
      };
      mpd.port = mkOption {
        type = types.int;
        default = 6600;
        description = "Port number MPD listens on";
      };
      mpd.collections = mkOption {
        type = types.attrs;
        default = { };
        description = "Music collections mounts into MPD music directory";
      };
      youtubeFrontends.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable non-web Youtube frontends";
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
      dev.projectenv.templates.entries = {
        "media.common" = configPrefix config.navigation.bookmarks.workspaces.roots "modules/content/media/templates/common";
      };

      environment.systemPackages = with pkgs; [ clipgrab freetube moc ncmpcpp ytfzf ];

      home-manager.users."${user}" = {
        # TODO: deal with converting from `webm` ^^^ (use ffmpeg btw)
        programs.mpv = {
          # TODO: consider extracting options
          enable = true;
          scripts = with pkgs.mpvScripts; [ youtube-quality ] ++ lib.optionals cfg.mpris.enable [ mpris ];
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
            osc = "no";
            # # Always use 1080p+ or 60 fps where available. Prefer VP9
            # # over AVC and VP8 for high-resolution streams.
            # ytdl=yes
            # ytdl-format=(bestvideo[ext=webm]/bestvideo[height>720]/bestvideo[fps=60])[tbr<13000]+(bestaudio[acodec=opus]/bestaudio[ext=webm]/bestaudio)/best
          };
        };
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.video "mpv.desktop";
        programs.zsh.shellAliases = { yg = "${pkgs.you-get}/bin/you-get"; };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/user/content.yml".text = ''
          name: content
          parent: default
          filter_title: ".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*"

          matches:
            - trigger: ":yt"
              replace: "nix shell \"nixpkgs#yt-dlp\" -c yt-dlp \"$|$\""

            - trigger: ":4yt"
              replace: "nix shell \"nixpkgs#yt-dlp\" -c yt-dlp \"$|$\" --merge-output-format mp4"

            - trigger: ":aft"
              replace: "nix shell \"nixpkgs#android-file-transfer\" -c android-file-transfer"
        '';
      };
    })
    (mkIf (cfg.enable && cfg.mpd.enable) {
      # TODO: setup mpris support (https://github.com/natsukagami/mpd-mpris)
      services.mpd = {
        enable = true;
        startWhenNeeded = true;
        extraConfig = ''
          audio_output {
            type     "pulse"
            name     "Pulseaudio"
            server   "127.0.0.1"
          }
        '';
      };

      fileSystems = mapAttrs'
        (token: srcPath:
          nameValuePair "${config.services.mpd.dataDir}/music/${token}" {
            device = srcPath;
            options = [ "bind" ];
          })
        cfg.mpd.collections;

      home-manager.users."${user}" = { home.packages = with pkgs; [ ario sonata cantata ]; };
    })
    (mkIf (cfg.enable && cfg.youtubeFrontends.enable) {
      # TODO: try https://github.com/trizen/youtube-viewer
      home-manager.users."${user}" = {
        # NOTE: default quotas seems inappropriate to use them freely
        home.packages = with pkgs; [ mps-youtube minitube smtube youtube-dl ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.wsMapping.rules = [
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
          key = [ prefixAlt "XF86AudioNext" ];
          cmd =
            "${pkgs.playerctl}/bin/playerctl --all-players position ${builtins.toString cfg.playback.deltaSeconds}+";
          mode = "root";
        }
        {
          key = [ prefixAlt "XF86AudioPrev" ];
          cmd =
            "${pkgs.playerctl}/bin/playerctl --all-players position ${builtins.toString cfg.playback.deltaSeconds}-";
          mode = "root";
        }
        {
          key = [ prefix "Control" "z" ];
          cmd = "${pkgs.mpc_cli}/bin/mpc prev";
          mode = "root";
        }
        {
          key = [ prefix "Control" "x" ];
          cmd = "${pkgs.mpc_cli}/bin/mpc play";
          mode = "root";
        }
        {
          key = [ prefix "Control" "c" ];
          cmd = "${pkgs.mpc_cli}/bin/mpc toggle";
          mode = "root";
        }
        {
          key = [ prefix "Control" "v" ];
          cmd = "${pkgs.mpc_cli}/bin/mpc stop";
          mode = "root";
        }
        {
          key = [ prefix "Control" "b" ];
          cmd = "${pkgs.mpc_cli}/bin/mpc next";
          mode = "root";
        }
      ];
    })
  ];
}
