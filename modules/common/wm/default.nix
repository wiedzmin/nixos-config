{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.wmCommon;
in {
  options = {
    wmCommon = {
      enable = mkOption {
        type = types.bool;
        default = config.wm.xmonad.enable; # TODO: add entries `or` later
        description = "Whether to enable common WM infrastructure.";
      };
      keys = mkOption {
        type = types.attrs;
        default = { };
        description = "Keybindings map.";
      };
      workspaces.primary = mkOption {
        type = types.attrs;
        default = {
          "web" = {
            key = "F1";
            transient = false;
          };
          "web2" = {
            key = "1";
            transient = true;
          };
          "web3" = {
            key = "`";
            transient = true;
          };
          "web4" = {
            key = "F6";
            transient = true;
          };
          "work" = {
            key = "F2";
            transient = false;
          };
          "tools" = {
            key = "F4";
            transient = false;
          };
          "scan" = {
            key = "F5";
            transient = false;
          };
        };
        description = "Workspaces to pin on primary screen, if attached.";
      };
      workspaces.secondary = mkOption {
        type = types.attrs;
        default = {
          "shell" = {
            key = "F3";
            transient = false;
          };
          "read" = {
            key = "4";
            transient = false;
          };
          "media" = {
            key = "5";
            transient = false;
          };
          "im" = {
            key = "c";
            transient = false;
          };
          "work2" = {
            key = "2";
            transient = true;
          };
          "work3" = {
            key = "3";
            transient = true;
          };
        };
        description = "Workspaces to pin on secondary screen, if attached.";
      };
      workspaces.tertiary = mkOption {
        type = types.attrs;
        default = {
          "scratch" = {
            key = "Esc";
            transient = false;
          };
        };
        description = "Workspaces to pin on tertiary screen, if attached.";
      };

      fonts.default = mkOption {
        type = types.str;
        default = "";
        description = "Current WM `internal` default font' definition.";
      };
      fonts.dmenu = mkOption {
        type = types.str;
        default = "";
        description = "Current WM `internal` dmenu font' definition.";
      };
      fonts.statusbar = mkOption {
        type = types.str;
        default = "";
        description = "Current WM `internal` statusbar font' definition.";
      };
      keybindingsCachePath = mkOption {
        type = types.str;
        default = homePrefix "keybindings.list";
        description = "Path to file with cached keybindings.";
        visible = false;
        internal = true;
        readOnly = true;
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        keybindings = writePythonScriptWithPythonPackages "keybindings" [ pkgs.python3Packages.redis pkgs.yad ]
          (builtins.readFile
            (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./keybindings.py; })));
      };
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.activation.purgeKeybindingsCache = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "rm -f ${cfg.keybindingsCachePath}";
        };
        home.packages = with pkgs; [ keybindings ];
      };
      wmCommon.keys = { "M-k" = { cmd = "keybindings"; }; };
    })
  ];
}
