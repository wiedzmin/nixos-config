{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# npkg#mpv

let
  cfg = config.content.media;
  user = config.attributes.mainUser.name;
  ID = config.attributes.mainUser.ID;
  inherit (config.wmCommon) prefix prefixAlt;
  yaml = pkgs.formats.yaml { };
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  nixpkgs-last-unbroken = import inputs.nixpkgs-last-unbroken {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
  nixpkgs-qtwebengine-bin = import inputs.nixpkgs-qtwebengine-bin {
    config = config.nixpkgs.config // {
      allowUnfree = true;
      permittedInsecurePackages = config.ext.nix.core.permittedInsecurePackages;
    };
    localSystem = { system = "x86_64-linux"; };
  };
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
      downloads.ordering.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable sorting of downloaded content";
      };
      downloads.ordering.timespec = mkOption {
        type = types.str;
        default = "";
        description = "Timestamp of service activation (in systemd format).";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      environment.systemPackages = with pkgs; [ nixpkgs-qtwebengine-bin.clipgrab freetube nixpkgs-last-unbroken.moc ncmpcpp ytfzf ];

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
    (mkIf (cfg.enable && cfg.downloads.ordering.enable) {
      attributes.download.sortingRules = {
        "(?P<basename>.*)\\.HEIC" = "pics";
        "(?P<basename>.*)\\.MOV" = "video";
        "(?P<basename>.*)\\.MP4" = "video";
        "(?P<basename>.*)\\.PDF" = "pdf";
        "(?P<basename>.*)\\.cer" = "creds";
        "(?P<basename>.*)\\.csv" = "csv";
        "(?P<basename>.*)\\.doc" = "docs";
        "(?P<basename>.*)\\.docx" = "docs";
        "(?P<basename>.*)\\.epub" = "books";
        "(?P<basename>.*)\\.gz" = "archives";
        "(?P<basename>.*)\\.html" = "markup";
        "(?P<basename>.*)\\.ics" = "cal";
        "(?P<basename>.*)\\.jpeg" = "pics";
        "(?P<basename>.*)\\.jpg" = "pics";
        "(?P<basename>.*)\\.json" = "markup";
        "(?P<basename>.*)\\.m4a" = "voice";
        "(?P<basename>.*)\\.mov" = "video";
        "(?P<basename>.*)\\.mp3" = "audio";
        "(?P<basename>.*)\\.mp4" = "video";
        "(?P<basename>.*)\\.odt" = "docs";
        "(?P<basename>.*)\\.org" = "org";
        "(?P<basename>.*)\\.pdf" = "pdf";
        "(?P<basename>.*)\\.png" = "pics";
        "(?P<basename>.*)\\.rtf" = "docs";
        "(?P<basename>.*)\\.tar" = "archives";
        "(?P<basename>.*)\\.tgz" = "archives";
        "(?P<basename>.*)\\.torrent" = "torrent";
        "(?P<basename>.*)\\.txt" = "texts";
        "(?P<basename>.*)\\.webm" = "video";
        "(?P<basename>.*)\\.webp" = "pics";
        "(?P<basename>.*)\\.xlsx" = "docs";
        "(?P<basename>.*)\\.xml" = "markup";
        "(?P<basename>.*)\\.yaml" = "markup";
        "(?P<basename>.*)\\.yml" = "markup";
        "(?P<basename>.*)\\.zip" = "archives";
        ".*cert.*" = "creds";
        ".*org\\.chromium.*" = "chromium";
        ".*record.*\\.wav" = "voice";
        ".*royallib.*" = "books";
      };
      home-manager.users."${user}" =
        let
          downloadsBaseSettings = {
            to = "";
            unmatched = {
              subdir = "unsorted";
              skip = false;
            };
            rules = config.attributes.download.sortingRules;
          };
        in
        {
          xdg.configFile = {
            "toolbox/downloads.json".text = builtins.toJSON (downloadsBaseSettings // {
              title = "downloads";
              from = config.attributes.download.path.browser;
            });
            "toolbox/telegram_downloads.json".text = builtins.toJSON (downloadsBaseSettings // {
              title = "telegram downloads";
              from = config.attributes.download.path.telegram;
            });
          };
        };
      systemd.user.services = builtins.listToAttrs (forEach [ "downloads" "telegram downloads" ] (title:
        let
          token = concatStringsSep "-" (splitString " " title);
          confName = concatStringsSep "_" (splitString " " title);
        in
        {
          name = "order-${token}";
          value = {
            description = "${lib.toSentenceCase title} ordering";
            wantedBy = [ "graphical.target" ];
            partOf = [ "graphical.target" ];
            serviceConfig = {
              Type = "oneshot";
              Environment = [ "DEBUG_MODE=1" ];
              ExecStart = "${nurpkgs.toolbox}/bin/orderfiles --config ${xdgConfig user "/toolbox/${confName}.json"}";
              StandardOutput = "journal";
              StandardError = "journal";
            };
          };
        }));
      systemd.user.timers = builtins.listToAttrs (forEach [ "downloads" "telegram downloads" ] (title:
        let
          token = concatStringsSep "-" (splitString " " title);
        in
        {
          name = "order-${token}";
          value = renderTimer "${lib.toSentenceCase title} ordering" "" "" cfg.downloads.ordering.timespec false "";
        }));
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
        home.packages = with pkgs; [ sonata cantata ];
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
          url = "https://github.com/mpv-player/mpv/blob/master/etc/input.conf";
          browseWith = appCmdFull config.attributes.browser.default.traits;
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
          url = "https://mpv.io/manual/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
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
          url = "https://www.google.ru/";
          browseWith = appCmdFull config.attributes.browser.default.traits;
          searchSuffix = "?q=pdf+";
        };
        "camdata" = {
          desc = "Camera content workdir";
          path = homePrefix user "blobs/work/camdata";
        };
      };
    })
  ];
}
