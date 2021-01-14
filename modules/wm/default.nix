{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with import ../wmutil.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.wmCommon;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  dmenu_select_windows =
    mkShellScriptWithDeps "dmenu_select_windows" (with pkgs; [ coreutils nurpkgs.dmenu-ng wmctrl ]) ''
      wmctrl -a $(wmctrl -l | cut -d" " -f5- | dmenu -i -l 15 -fn '${config.wmCommon.fonts.dmenu}')
    '';
in {
  options = {
    wmCommon = {
      enable = mkOption {
        type = types.bool;
        default = config.wm.xmonad.enable || config.wm.stumpwm.enable;
        description = "Whether to enable common WM infrastructure.";
      };
      workspaces = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
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
        description = ''
          Window-to-workspace mapping rules.

          Use example below for negative filtering (currently i3 only):
          <title|class|instance>="^(?!<substring>).*$"
        '';
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
      kbdd.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Autostart kbdd daemon.";
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
        keybindings = mkPythonScriptWithDeps "keybindings" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis yad ])
          (readSubstituted ../subst.nix ./scripts/keybindings.py);
      };
      home-manager.users.${user} = {
        home.activation.purgeKeybindingsCache = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "rm -f ${cfg.keybindingsCachePath}";
        };
        home.packages = with pkgs; [ keybindings ];
      };
      wmCommon.autostart.entries = optionals (cfg.kbdd.enable) [ "${pkgs.kbdd}/bin/kbdd" ];
      wmCommon.keys = [
        {
          key = [ cfg.prefix "k" ];
          cmd = "${pkgs.keybindings}/bin/keybindings";
          mode = "root";
        }
        {
          key = [ "w" ];
          cmd = "${dmenu_select_windows}/bin/dmenu_select_windows";
          mode = "select";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ keybindings ]; };
    })
  ];
}
