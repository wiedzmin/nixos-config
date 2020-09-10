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
            transient = false;
            type = "primary";
          }
          {
            name = "web2";
            key = [ "1" ];
            transient = true;
            type = "primary";
          }
          {
            name = "web3";
            key = [ "`" ];
            transient = true;
            type = "primary";
          }
          {
            name = "web4";
            key = [ "F6" ];
            transient = true;
            type = "primary";
          }
          {
            name = "work";
            key = [ "F2" ];
            transient = false;
            type = "primary";
          }
          {
            name = "tools";
            key = [ "F4" ];
            transient = false;
            type = "primary";
          }
          {
            name = "scan";
            key = [ "F5" ];
            transient = false;
            type = "primary";
          }
          {
            name = "shell";
            key = [ "F3" ];
            transient = false;
            type = "secondary";
          }
          {
            name = "read";
            key = [ "4" ];
            transient = false;
            type = "secondary";
          }
          {
            name = "media";
            key = [ "5" ];
            transient = false;
            type = "secondary";
          }
          {
            name = "im";
            key = [ "c" ];
            transient = false;
            type = "secondary";
          }
          {
            name = "work2";
            key = [ "2" ];
            transient = true;
            type = "primary";
          }
          {
            name = "work3";
            key = [ "3" ];
            transient = true;
            type = "secondary";
          }
          {
            name = "scratch";
            key = [ "Esc" ];
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
      wsMapping.rules = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Window-to-workspace mapping rules.";
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
      autostart.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Start some applications automatically.";
      };
      autostart.entries = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Applications to start automatically.";
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
        keybindings = mkPythonScriptWithDeps "keybindings" (with pkgs; [ pystdlib python3Packages.redis yad ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/keybindings.py; })));
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
