{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
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
        default = config.wm.i3.enable || config.wm.awesome.enable;
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
      prefixAlt = mkOption {
        type = types.str;
        default = "Mod1";
        description = "WM alternate prefix key";
      };
      keybindings.entries = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Common keybindings.";
      };
      keybindings.help.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable showing WM keybindings help, using respective script";
      };
      autorepeat.delay = mkOption {
        type = types.int;
        default = 280;
        description = "Autorepeat delay for xset";
      };
      autorepeat.rate = mkOption {
        type = types.int;
        default = 30;
        description = "Autorepeat rate for xset";
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
      fonts.simple = mkOption {
        type = types.str;
        default = "";
        example = "Iosevka 14";
        description = "Simplified default font' definition, as used, for example. by AwesomeWM";
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
      fonts.family.size.large = mkOption {
        type = types.str;
        default = "";
        description = "System-wide font representation in '<family>:<size>' format, large version for OSD use";
      };
      fonts.xlfd.large = mkOption {
        type = types.str;
        default = "";
        description = "System-wide XLFD font representation, large version for OSD use";
      };
      fonts.family.common = mkOption {
        type = types.str;
        default = "";
        description = "System-wide font common family";
      };
      fonts.family.deadd = mkOption {
        type = types.str;
        default = "";
        description = "System-wide font family for `deadd notification system` notifier";
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
      focus.show = mkOption {
        type = types.bool;
        default = true;
        description = "Visually denote currently focused window";
      };
      focus.followsMouse = mkOption {
        type = types.bool;
        default = false;
        description = "Whether focus should follow mouse";
      };
      focus.list.useWMSpecific = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to use WM-specific scripts/tools for windows listing-->focusing";
      };
      autostart.entries = mkOption {
        type = types.listOf types.attrs;
        default = [ ];
        description = "Applications to start automatically.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      attributes.wms.enabled = true;

      services.xserver = {
        enable = true;
        autorun = true;
        desktopManager = {
          xterm.enable = false;
        };
        displayManager.sessionCommands = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          ${pkgs.wmname}/bin/wmname LG3D
          ${pkgs.xorg.xset}/bin/xset r rate ${builtins.toString cfg.autorepeat.delay} ${builtins.toString cfg.autorepeat.rate}
        '';
      };

      systemd.user.services.kbdd = optionalAttrs cfg.kbdd.enable {
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

      wmCommon.modeBindings = {
        "dev" = [ cfg.prefix "d" ];
        "layout" = [ cfg.prefix "<" ];
        "resize" = [ cfg.prefix "=" ];
        "run" = [ cfg.prefix "r" ];
        "select" = [ cfg.prefix "." ];
        "services" = [ cfg.prefix "s" ];
        "xserver" = [ cfg.prefix "x" ];
      };
      wmCommon.keybindings.entries = optionals (!cfg.focus.list.useWMSpecific) [
        {
          key = [ "w" ];
          cmd = "${pkgs.rofi}/bin/rofi -modi window -show";
          mode = "select";
        }
      ] ++ optionals (cfg.keybindings.help.enable) [
        {
          key = [ cfg.prefix "k" ];
          cmd = "${nurpkgs.toolbox}/bin/wmkb keys --fuzzy";
          mode = "root";
        }
        {
          key = [ cfg.prefix "Shift" "k" ];
          cmd = "${nurpkgs.toolbox}/bin/wmkb keys --tree";
          mode = "root";
        }
        {
          key = [ cfg.prefix "m" ];
          cmd = "${nurpkgs.toolbox}/bin/wmkb modes --fuzzy";
          mode = "root";
        }
        (goLocalDebugKeybinding config {
          key = [ cfg.prefix "t" ];
          cmd = [ "wmkb" "workspaces" "--fuzzy" ];
          mode = "root";
        })
      ];
      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set wm/workspaces ${mkRedisJSON (foldl (a: b: a // { "${b.name}" = builtins.head b.key; }) { } cfg.workspaces)}

        ${pkgs.redis}/bin/redis-cli set wm/window_rules ${
          lib.strings.escapeNixString (builtins.toJSON (builtins.filter
            (r: !builtins.hasAttr "scratchpad" r || (builtins.hasAttr "scratchpad" r && !r.scratchpad))
            (lib.forEach (windowRulesFromBookmarks config.navigation.bookmarks.entries ++ cfg.wsMapping.rules) prepareWindowRule)))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.focus.show) {
      assertions = [{
        assertion = config.workstation.video.transparency.enable;
        message = "wm: common: `flashfocus` requires window compositor on X";
      }];

      wmCommon.autostart.entries = [{ cmd = "${pkgs.flashfocus}/bin/flashfocus"; }];
      wmCommon.keybindings.entries = [{
        key = [ "f" ];
        cmd = "${pkgs.flashfocus}/bin/focus_window";
        mode = "xserver";
      }];
      workstation.video.transparency = {
        opacityRules = [ "0:_NET_WM_STATE@:32a *= '_NET_WM_STATE_HIDDEN'" ];
      };
    })
  ];
}
