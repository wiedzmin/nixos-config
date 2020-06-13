let
  deps = import ../../../nix/sources.nix;
  proposed = import deps.nixpkgs-proposed { config.allowUnfree = true; };
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.content;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.content = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable content-related tools.";
      };
      bookmarks.enable = mkOption {
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
        paste_to_ix = writePythonScriptWithPythonPackages "paste_to_ix" [ pkgs.ix pkgs.xsel ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./paste_to_ix.sh; })));
      };
      tools.ebooks.readers.roots = [ (homePrefix "bookshelf") ];
      services.clipmenu.enable = true;
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.activation.ensureMimeappsList = {
          after = [ ];
          before = [ "checkLinkTargets" ];
          data = "rm -f ${homePrefix ".config/mimeapps.list"} ";
        };

        # TODO: consider desktop files locating automation
        xdg.mimeApps.defaultApplications = (mapMimesToApp config.attributes.mimetypes.images "feh.desktop")
          // (mapMimesToApp config.attributes.mimetypes.video "mpv.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.docs "writer.desktop")
          // (mapMimesToApp config.attributes.mimetypes.office.spreadsheets "calc.desktop");
        home.file = {
          ".mpv/config".text = ''
            hwdec=vdpau
            hwdec-codecs=all

            vo=gpu,xv
            ao=pulse

            af=scaletempo
            audio-samplerate=48000

            slang = en
            alang = en,eng,us

            volume-max=200

            cache = yes
            cache-on-disk = yes
            cache-pause-initial = yes
            cache-pause-wait = 10

            # Always use 1080p+ or 60 fps where available. Prefer VP9
            # over AVC and VP8 for high-resolution streams.
            ytdl=yes
            ytdl-format=(bestvideo[ext=webm]/bestvideo[height>720]/bestvideo[fps=60])[tbr<13000]+(bestaudio[acodec=opus]/bestaudio[ext=webm]/bestaudio)/best
          '';
        } // lib.optionalAttrs (config.custom.shell.enable) {
          "tmuxp/media.yml".text = ''
            session_name: media
            windows:
              - window_name: mps-youtube
                panes:
                  - ${pkgs.mps-youtube}/bin/mpsyt
          '';
        };
        home.packages = with pkgs; [
          android-file-transfer
          jmtpfs # consider providing some (shell) automation
          saldl # consider providing some (shell) automation
          proposed.you-get
          # =======
          archiver
          # =======
          exiv2
          mediainfo
          # =======
          paste_to_ix

          monolith
          tartube
        ];
        services.syncthing.enable = true; # TODO: consider separate option(s)
        xdg.mimeApps.enable = true;
        programs.aria2.enable = true;
        programs.mpv = {
          enable = true;
          scripts = with pkgs.mpvScripts; [ mpris ];
        };
        programs.zsh.shellAliases = {
          yg = "${proposed.you-get}/bin/you-get";
        } // lib.optionalAttrs (config.custom.navigation.misc.enable) {
          pyg = "${pkgs.pueue}/bin/pueue add -- ${proposed.you-get}/bin/you-get";
        };
      };
    })
    (mkIf cfg.bookmarks.enable {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
        buku_add = writePythonScriptWithPythonPackages "buku_add" [
          nixpkgs-pinned-05_12_19.buku
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.notify2
          pkgs.xsel
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./buku_add.py; })));
        buku_search_tag = writeShellScriptBinWithDeps "buku_search_tag" [
          nixpkgs-pinned-05_12_19.buku
          pkgs.coreutils
          pkgs.dmenu
          pkgs.gawk
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./buku_search_tag.sh; })));
        buku_search_url =
          writeShellScriptBinWithDeps "buku_search_url" [ nixpkgs-pinned-05_12_19.buku pkgs.coreutils pkgs.dmenu ]
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./buku_search_url.sh; })));
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
          writeShellScriptBinWithDeps "screenshot_active_window" [ pkgs.coreutils pkgs.maim pkgs.xclip pkgs.xdotool ]
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./screenshot_active_window.sh; })));
        screenshot_full = writeShellScriptBinWithDeps "screenshot_full" [ pkgs.coreutils pkgs.maim pkgs.xclip ]
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./screenshot_full.sh; })));
        screenshot_region = writeShellScriptBinWithDeps "screenshot_region" [ pkgs.coreutils pkgs.maim pkgs.xclip ]
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./screenshot_region.sh; })));
      };

      environment.systemPackages = with pkgs; [ screenshot_active_window screenshot_full screenshot_region ];
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
    (mkIf (cfg.enable && cfg.wm.enable && cfg.bookmarks.enable) {
      wmCommon.keys = [{
        key = "m";
        cmd = "${pkgs.buku_add}/bin/buku_add";
        mode = "run";
      }];
    })
    (mkIf (cfg.enable && cfg.wm.enable && cfg.screenshots.enable) {
      wmCommon.keys = [
        {
          key = "Print";
          cmd = "${pkgs.screenshot_active_window}/bin/screenshot_active_window";
          mode = "root";
        }
        {
          key = "Control+Print";
          cmd = "${pkgs.screenshot_full}/bin/screenshot_full";
          mode = "root";
        }
        {
          key = "${prefix}+Print";
          cmd = "${pkgs.screenshot_region}/bin/screenshot_region";
          mode = "root";
        }
      ];
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [{
        key = "p";
        cmd = "${pkgs.paste_to_ix}/bin/paste_to_ix";
        mode = "window";
      }];
    })
  ];
}
