{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let
  cfg = config.custom.content;
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
      bookmarks.batchOpenTreshold = mkOption {
        type = types.int;
        default = 20; # TODO: add assertion with upper bound
        description = "Maximum bookmarks search results count to be opened in batch";
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
      warmup.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable pulling some highly used data into memory.";
      };
      warmup.paths = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "List of paths to warm up.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        paste_to_ix = pkgs.writeShellScriptBin "paste_to_ix"
          (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./paste_to_ix.sh; })));
      };
      services.clipmenu.enable = true;
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          android-file-transfer
          aria2
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
        ];
        services.syncthing.enable = true; # TODO: consider separate option(s)
        programs.mpv = {
          enable = true;
          scripts = with pkgs.mpvScripts; [
            mpris
          ];
        };
      };
    })
    (mkIf cfg.bookmarks.enable {
      nixpkgs.config.packageOverrides = _: rec {
        # FIXME: use ideas from https://github.com/mitchweaver/bin/blob/5bad2e16006d82aeeb448f7185ce665934a9c242/util/pad
        buku_add = writePythonScriptWithPythonPackages "buku_add" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.notify2
        ] (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./buku_add.py; })));
        buku_search_tag = pkgs.writeScriptBin "buku_search_tag"
          (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./buku_search_tag.sh; })));
        buku_search_url = pkgs.writeScriptBin "buku_search_url"
          (builtins.readFile
            (pkgs.substituteAll
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
        screenshot_active_window = pkgs.writeScriptBin "screenshot_active_window"
          (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./screenshot_active_window.sh; })));
        screenshot_full = pkgs.writeScriptBin "screenshot_full"
          (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./screenshot_full.sh; })));
        screenshot_region = pkgs.writeScriptBin "screenshot_region"
          (builtins.readFile
            (pkgs.substituteAll
              ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./screenshot_region.sh; })));
      };

      environment.systemPackages = with pkgs; [
        screenshot_active_window
        screenshot_full
        screenshot_region
      ];
    })
    (mkIf (cfg.warmup.enable && cfg.warmup.paths != []) {
      systemd.user.services."warmup" = {
        description = "Warm up paths";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${pkgs.vmtouch}/bin/vmtouch -t ${lib.concatStringsSep " " cfg.warmup.paths}";
          StandardOutput = "journal+console";
          StandardError = "inherit";
        };
        partOf = [ "multi-user.target" ]; # FIXME: does not autostart
        wantedBy = [ "multi-user.target" ];
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable && cfg.bookmarks.enable) {
      wm.xmonad.keybindings = {
        "M-y" = ''spawn "${pkgs.buku_add}/bin/buku_add"'';
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable && cfg.screenshots.enable) {
      wm.xmonad.keybindings = {
        "<Print>" = ''spawn "${pkgs.screenshot_active_window}/bin/screenshot_active_window"'';
        "C-<Print>" = ''spawn "${pkgs.screenshot_full}/bin/screenshot_full"'';
        "M-<Print>" = ''spawn "${pkgs.screenshot_region}/bin/screenshot_region"'';
      };
    })
    (mkIf (cfg.enable && cfg.xmonad.enable) {
      wm.xmonad.keybindings = {
        "M-i" = ''spawn "${pkgs.paste_to_ix}/bin/paste_to_ix"'';
      };
    })
  ];
}
