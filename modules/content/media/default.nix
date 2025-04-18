{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.content.media;
  user = config.attributes.mainUser.name;
  ID = config.attributes.mainUser.ID;
  inherit (config.wmCommon) prefix prefixAlt;
  yaml = pkgs.formats.yaml { };
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
      mpv.osc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable OSC for MPV";
      };
      youtubeFrontends.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable non-web Youtube frontends";
      };
      ffmpeg.workdir = mkOption {
        type = types.str;
        default = "/home/alex3rd/blobs/work/camdata"; # FIXME: parameterize
        description = "Predefined working directory for various ffmpeg operations";
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable media-related bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ clipgrab freetube moc ncmpcpp ytfzf ];

      home-manager.users."${user}" = {
        programs.mpv = {
          enable = true;
          scripts = with pkgs.mpvScripts; [ quality-menu ] ++ lib.optionals cfg.mpris.enable [ mpris ];
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
          } // lib.optionalAttrs cfg.mpv.osc.enable {
            osd-level = 2;
            osc = "yes";
          } // lib.optionalAttrs (!cfg.mpv.osc.enable) {
            osc = "no";
          };
        };
        programs.fish.functions = {
          # TODO: consider replicating ffmpeg expansions also as Fish functions
          screencast = {
            # NOTE: see http://trac.ffmpeg.org/wiki/Capture/Desktop for reference
            description = "Screen capture using ffmpeg";
            # TODO: try elaborating Nix expression for https://github.com/scottkirkwood/key-mon or find and play with similar utilities
            body = ''
              mkdir -p /tmp/record
              ffmpeg -probesize 3000000000 -f x11grab -framerate 25 -s $(redis-cli get wm/dimensions) -i :0.0 -vcodec libx264 -threads 2 -preset ultrafast -crf 0 /tmp/record/record-(date +"%FT%T%:z").mp4
            '';
          };
        };
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.video "mpv.desktop";
        programs.zsh.shellAliases = { yg = "${pkgs.you-get}/bin/you-get"; };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          android-file-transfer
          ffmpeg-full
          yt-dlp
        ];
        xdg.configFile = {
          "espanso/match/content.yml".source = yaml.generate "espanso-content.yml"
            {
              matches = [
                {
                  trigger = ":yt";
                  replace = "yt-dlp \"$|$\"";
                }
                {
                  trigger = ":4yt";
                  replace = "yt-dlp \"$|$\" --merge-output-format mp4";
                }
                {
                  trigger = ":aft";
                  replace = "android-file-transfer";
                }
                {
                  trigger = ":ffmc";
                  replace = "cd ${cfg.ffmpeg.workdir} && ffmpeg -i {{inputfile.value}} -c:v copy -c:a copy -f mp4 {{namesansext.value}}.mp4 && mv {{inputfile.value}} ${cfg.ffmpeg.workdir}/done && echo \"file '${cfg.ffmpeg.workdir}/{{namesansext.value}}.mp4'\">> ${cfg.ffmpeg.workdir}/files.list";
                  vars = [
                    {
                      name = "mtsfiles";
                      type = "shell";
                      params = { cmd = "ls ${cfg.ffmpeg.workdir} | grep -e \"[Mm][Tt][Ss]\""; };
                    }
                    {
                      name = "inputfile";
                      type = "choice";
                      params = { values = "{{mtsfiles}}"; };
                    }
                    {
                      name = "namesansext";
                      type = "shell";
                      params = { cmd = "echo \"{{inputfile.value}}\" | cut -d'.' -f1"; };
                    }
                  ];
                }
                {
                  trigger = ":ffmg";
                  replace = "cd ${cfg.ffmpeg.workdir} && ffmpeg -f concat -safe 0 -i files.list -c copy result.mp4"; # FIXME: consider parameterizing output filename in some way
                }
              ];
            };
        } // lib.optionalString (config.shell.core.queueing.enable) {
          "espanso/match/content_queueing.yml".source = yaml.generate "espanso-content_queueing.yml"
            {
              matches = [
                {
                  trigger = ":pyt";
                  replace = "pueue add 'yt-dlp \"$|$\"'";
                }
                {
                  trigger = ":p4yt";
                  replace = "pueue add 'yt-dlp \"$|$\" --merge-output-format mp4'";
                }
              ];
            };
        };
      };
    })
    (mkIf (cfg.enable && cfg.mpd.enable) {
      # TODO: setup mpris support (https://github.com/natsukagami/mpd-mpris)
      services.mpd = {
        enable = true;
        startWhenNeeded = true;
        extraConfig = ''
          audio_output {
            type     "pipewire"
            name     "Pipewire"
            server   "127.0.0.1"
          }
        '';
        user = user;
      };
      systemd.services.mpd.environment = {
        # https://gitlab.freedesktop.org/pipewire/pipewire/-/issues/609
        XDG_RUNTIME_DIR = "/run/user/${ID}"; # Must match above user. MPD will look inside this directory for the PipeWire socket.
      };

      fileSystems = mapAttrs'
        (token: srcPath:
          nameValuePair "${config.services.mpd.dataDir}/music/${token}" {
            device = srcPath;
            options = [ "bind" ];
          })
        cfg.mpd.collections;

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ ario sonata cantata ];
        programs.qutebrowser = {
          keyBindings = {
            normal = {
              "ym" = "spawn ${pkgs.mpc_cli}/bin/mpc add yt:{url}";
              "нь" = "spawn ${pkgs.mpc_cli}/bin/mpc add yt:{url}";
            };
          };
        };
      };
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
      wmCommon.keybindings.entries = [
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
    (mkIf (cfg.enable && cfg.bookmarks.enable && config.navigation.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "mpv-keys" = {
          desc = "mpv default keybindings reference";
          remote.url = "https://github.com/mpv-player/mpv/blob/master/etc/input.conf";
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "github mpv-player";
              desktop = "main"; # [ref:desktop_main]
            }
          ];
        };
        "mpv-manual" = {
          desc = "mpv manual";
          remote.url = "https://mpv.io/manual/";
          windowRules = [
            {
              class = mkWSMappingBrowsersRegexp config.attributes.browser;
              title = "mpv manual";
              desktop = "main"; # [ref:desktop_main]
            }
          ];
        };
        "pdf-searching" = {
          desc = "pdf + ";
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=pdf+";
          };
        };
        "camdata" = {
          desc = "Camera content workdir";
          local.path = homePrefix user "blobs/work/camdata";
        };
      };
    })
  ];
}
