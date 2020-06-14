{ config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with import ./wmutil.nix { inherit config lib pkgs; };
with lib;

let cfg = config.wmCommon;
in {
  options = {
    wmCommon = {
      enable = mkOption {
        type = types.bool;
        default = config.wm.xmonad.enable || config.wm.i3.enable || config.wm.stumpwm.enable;
        description = "Whether to enable common WM infrastructure.";
      };
      workspaces = mkOption {
        type = types.listOf types.attrs;
        default = [
          {
            name = "web";
            key = [ "F1" ];
            index = 1;
            transient = false;
            type = "primary";
          }
          {
            name = "web2";
            key = [ "1" ];
            index = 7;
            transient = true;
            type = "primary";
          }
          {
            name = "web3";
            key = [ "`" ];
            index = 8;
            transient = true;
            type = "primary";
          }
          {
            name = "web4";
            key = [ "F6" ];
            index = 10;
            transient = true;
            type = "primary";
          }
          {
            name = "work";
            key = [ "F2" ];
            index = 2;
            transient = false;
            type = "primary";
          }
          {
            name = "tools";
            key = [ "F4" ];
            index = 5;
            transient = false;
            type = "primary";
          }
          {
            name = "scan";
            key = [ "F5" ];
            index = 6;
            transient = false;
            type = "primary";
          }
          {
            name = "shell";
            key = [ "F3" ];
            index = 3;
            transient = false;
            type = "secondary";
          }
          {
            name = "read";
            key = [ "4" ];
            index = 4;
            transient = false;
            type = "secondary";
          }
          {
            name = "media";
            key = [ "5" ];
            index = 11;
            transient = false;
            type = "secondary";
          }
          {
            name = "im";
            key = [ "c" ];
            index = 12;
            transient = false;
            type = "secondary";
          }
          {
            name = "work2";
            key = [ "2" ];
            index = 13;
            transient = true;
            type = "secondary";
          }
          {
            name = "work3";
            key = [ "3" ];
            index = 14;
            transient = true;
            type = "secondary";
          }
          {
            name = "scratch";
            key = [ "Esc" ];
            index = 15;
            transient = false;
            type = "tertiary";
          }
        ];
        description = "Workspaces metadata.";
      };
      prefix = mkOption {
        type = types.str;
        default = "Mod4";
        description = "WM prefix key";
      };
      keys = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Common keybindings.";
      };
      modeBindings = mkOption {
        type = types.attrs;
        default = { };
        description = "Modes keybindings.";
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
      wmCommon.keys = [{
        key = [ cfg.prefix "k" ];
        cmd = "keybindings";
        mode = "root";
      }];
    })
  ];
}
