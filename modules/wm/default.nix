{ config, inputs, lib, pkgs, ... }:
with import ../util.nix { inherit config inputs lib pkgs; };
with import ../wmutil.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.wmCommon;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
in
{
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
      showfocus = mkOption {
        type = types.bool;
        default = true;
        description = "Visually denote currently focused window";
      };
      autostart.entries = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Applications to start automatically.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        keybindings = mkPythonScriptWithDeps "keybindings" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis yad ])
          (builtins.readFile ./scripts/keybindings.py);
      };

      attributes.wms.enabled = true;

      services.xserver = {
        desktopManager = {
          xterm.enable = false;
          gnome.enable = false;
        };
        displayManager.sessionCommands = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          ${pkgs.wmname}/bin/wmname LG3D
        '';
      };

      systemd.user.services.kbdd = optionalAttrs (cfg.kbdd.enable) {
        description = "KBDD";
        after = [ "graphical-session-pre.target" ];
        partOf = [ "graphical-session.target" ];
        wantedBy = [ "graphical-session.target" ];
        serviceConfig = {
          Type = "dbus";
          BusName = "ru.gentoo.KbddService";
          ExecStart = "${pkgs.kbdd}/bin/kbdd -n";
          StandardOutput = "journal";
          StandardError = "journal";
        };
      };

      home-manager.users.${user} = {
        home.packages = with pkgs; [ keybindings ];
      };
      wmCommon.keys = [
        {
          key = [ cfg.prefix "k" ];
          cmd = "${pkgs.keybindings}/bin/keybindings";
          mode = "root";
        }
        {
          key = [ cfg.prefix "Shift" "k" ];
          cmd = "${pkgs.keybindings}/bin/keybindings --fuzzy";
          mode = "root";
        }
        {
          key = [ "w" ];
          cmd = "${pkgs.rofi}/bin/rofi -modi window -show";
          mode = "select";
        }
      ];
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set wm/workspaces ${
          lib.strings.escapeNixString (builtins.toJSON (lib.forEach cfg.workspaces (w: w.name)))
        }

        ${pkgs.redis}/bin/redis-cli set wm/window_rules ${
          lib.strings.escapeNixString (builtins.toJSON (builtins.filter
            (r: !builtins.hasAttr "scratchpad" r || (builtins.hasAttr "scratchpad" r && r.scratchpad == false))
            (lib.forEach cfg.wsMapping.rules prepareWindowRule)))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.showfocus) {
      # TODO: review https://github.com/fennerm/flashfocus/wiki
      wmCommon.autostart.entries = [ "${pkgs.flashfocus}/bin/flashfocus" ];
      wmCommon.keys = [{
        key = [ "f" ];
        cmd = "${pkgs.flashfocus}/bin/focus_window";
        mode = "xserver";
      }];
      workstation.video.transparency = {
        extraOptions = [ "detect-client-opacity = true;" ];
        opacityRule = [ "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'" ];
      };
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = { home.packages = with pkgs; [ keybindings ]; };
    })
  ];
}
