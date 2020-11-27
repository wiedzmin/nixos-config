{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.custom.content;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.content = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-related tools.";
      };
      bookmarking.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable bookmarking harness";
      };
      players.deltaSeconds = mkOption {
        type = types.int;
        default = 10;
        description = "Player rewinding delta in seconds";
      };
      screenshots.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable screenshots functionality.";
      };
      screenshots.baseDir = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "Screenshots base directory";
      };
      screenshots.dateFormat = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "screenshot date suffix format";
      };
      urlRegex.py = mkOption {
        description = "Common URL regular expression";
        type = types.str;
        default = "(https?|ftp|file)://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]";
      };
      warmup.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable pulling some highly used data into memory.";
      };
      warmup.paths = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of paths to warm up.";
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
      nixpkgs.config.packageOverrides = _: rec {
        paste_to_ix = mkPythonScriptWithDeps "paste_to_ix" (with pkgs; [ ix xsel ])
          (readSubstituted ../subst.nix ./scripts/paste_to_ix.sh);
      };
      home-manager.users.${user} = {
        home.activation.ensureMimeappsList = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          data = "rm -f ${homePrefix ".config/mimeapps.list"} ";
        };

        xdg.dataHome = homePrefix ".local/share";
        xdg.userDirs = {
          enable = true;
          desktop = homePrefix "Desktop";
          documents = homePrefix "docs/inbox";
          download = homePrefix "Downloads";
          music = homePrefix "blobs/music";
          pictures = homePrefix "blobs/pics";
          videos = homePrefix "blobs/video";
        };

        # TODO: consider desktop files locating automation
        xdg.mimeApps.defaultApplications = (mapMimesToApp config.attributes.mimetypes.images "feh.desktop")
          // (mapMimesToApp config.attributes.mimetypes.video "mpv.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.docs "writer.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.spreadsheets "calc.desktop");

        home.packages = with pkgs; [
          android-file-transfer
          jmtpfs # consider providing some (shell) automation
          saldl # consider providing some (shell) automation
          you-get
          # =======
          archiver
          # =======
          exiv2
          mediainfo
          # =======
          paste_to_ix

          monolith
          tartube

          clipcat
        ];
        services.syncthing.enable = true; # TODO: consider separate option(s)
        xdg.mimeApps.enable = true;
        programs.aria2.enable = true;
        programs.mpv = {
          enable = true;
          scripts = with pkgs.mpvScripts; [ mpris sponsorblock ];
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

        xdg.configFile."clipcat/clipcatd.toml".text = toToml {
          daemonize = true;
          max_history = 50;
          history_file_path = "${hm.xdg.cacheHome}/clipcat/clipcatd/db";
          log_level = "INFO";
          monitor = {
            load_current = true;
            enable_clipboard = true;
            enable_primary = false;
          };
          grpc = {
            host = "127.0.0.1";
            port = 45045;
          };
        };
        xdg.configFile."clipcat/clipcatctl.toml".text = toToml {
          server_host = "127.0.0.1";
          server_port = 45045;
          log_level = "INFO";
        };
        xdg.configFile."clipcat/clipcat-menu.toml".text = toToml {
          server_host = "127.0.0.1";
          server_port = 45045;
          finder = "dmenu";
          dmenu = {
            line_length = 150;
            menu_length = 20;
          };
          custom_finder = {
            program = "${pkgs.fzf}/bin/fzf";
            args = [ ];
          };
        };
      };
    })
    (mkIf cfg.bookmarking.enable {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
        buku_add = mkPythonScriptWithDeps "buku_add"
          (with pkgs; [ buku nurpkgs.pystdlib xsel ])
          (readSubstituted ../subst.nix ./scripts/buku_add.py);
        buku_search_tag = mkShellScriptWithDeps "buku_search_tag"
          (with pkgs; [ coreutils nurpkgs.dmenu-ng gawk buku ])
          (readSubstituted ../subst.nix ./scripts/buku_search_tag.sh);
        buku_search_url = mkShellScriptWithDeps "buku_search_url"
          (with pkgs; [ coreutils nurpkgs.dmenu-ng buku ])
          (readSubstituted ../subst.nix ./scripts/buku_search_url.sh);
      };
    })
    (mkIf (cfg.enable && cfg.screenshots.enable) {
      assertions = [
        {
          assertion = cfg.screenshots.baseDir != null;
          message = "Must provide path to screenshots dir.";
        }
        {
          assertion = cfg.screenshots.dateFormat != null;
          message = "Must provide date format.";
        }
      ];

      nixpkgs.config.packageOverrides = _: rec {
        screenshot_active_window =
          mkShellScriptWithDeps "screenshot_active_window" (with pkgs; [ coreutils maim xclip xdotool ])
            (readSubstituted ../subst.nix ./scripts/screenshot_active_window.sh);
        screenshot_full = mkShellScriptWithDeps "screenshot_full" (with pkgs; [ coreutils maim xclip ])
          (readSubstituted ../subst.nix ./scripts/screenshot_full.sh);
        screenshot_region = mkShellScriptWithDeps "screenshot_region" (with pkgs; [ coreutils maim xclip ])
          (readSubstituted ../subst.nix ./scripts/screenshot_region.sh);
      };
    })
    (mkIf (cfg.warmup.enable && cfg.warmup.paths != [ ]) {
      systemd.user.services."warmup" = {
        description = "Warm up paths";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.vmtouch}/bin/vmtouch -t ${lib.concatStringsSep " " cfg.warmup.paths}";
          StandardOutput = "journal";
          StandardError = "journal";
        };
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
      };
    })
    (mkIf (cfg.enable && cfg.wm.enable && cfg.bookmarking.enable) {
      wmCommon.keys = [{
        key = [ "m" ];
        cmd = "${pkgs.buku_add}/bin/buku_add";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && cfg.wm.enable && cfg.screenshots.enable) {
      wmCommon.keys = [
        {
          key = [ "Print" ];
          cmd = "${pkgs.screenshot_active_window}/bin/screenshot_active_window";
          mode = "root";
        }
        {
          key = [ "Control" "Print" ];
          cmd = "${pkgs.screenshot_full}/bin/screenshot_full";
          mode = "root";
        }
        {
          key = [ prefix "Print" ];
          cmd = "${pkgs.screenshot_region}/bin/screenshot_region";
          mode = "root";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ "p" ];
          cmd = "${pkgs.paste_to_ix}/bin/paste_to_ix";
          mode = "window";
        }
        {
          key = [ "c" ];
          cmd = ''PATH="$PATH:${nurpkgs.dmenu-ng}/bin/" ${pkgs.clipcat}/bin/clipcat-menu insert''; # TODO: consider abstracting away
          mode = "select";
        }
        {
          key = [ "Shift" "c" ];
          cmd = ''PATH="$PATH:${nurpkgs.dmenu-ng}/bin/" ${pkgs.clipcat}/bin/clipcat-menu remove'';
          mode = "select";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [
          buku_add
          buku_search_tag
          buku_search_url
          paste_to_ix
          screenshot_active_window
          screenshot_full
          screenshot_region
        ];
      };
    })
  ];
}
