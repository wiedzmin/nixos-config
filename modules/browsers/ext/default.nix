{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.browsers.ext;
  user = config.attributes.mainUser.name;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
in
{
  options = {
    browsers.ext = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable navigation infra.
        '';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable customized navigation for Emacs.
        '';
      };
      emacs.browseUrlSetup = mkOption {
        type = types.lines;
        default = '''';
        visible = false;
        internal = true;
        description = "Specialized `browse-url` package setup";
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
      assertions = [
        {
          assertion = config.ext.networking.vpn.enable;
          message = "browsers/core: must enable vpn functionality.";
        }
        {
          assertion = config.workstation.systemtraits.enable;
          message = "browsers/core: must enable systemtraits maintainence.";
        }
      ];

      home-manager.users.${user} = { home.packages = with pkgs; [ rdrview ]; };

      workstation.systemtraits.instructions = with config.navigation.bookmarks; ''
        ${pkgs.redis}/bin/redis-cli set nav/webjumps ${
          lib.strings.escapeNixString
          (builtins.toJSON (remoteWebjumps (enabledRemotes entries) separator.fields separator.tags))
        }
        ${pkgs.redis}/bin/redis-cli set nav/searchengines ${
          lib.strings.escapeNixString
          (builtins.toJSON (remoteSearchEngines (enabledRemotes entries) separator.fields separator.tags))
        }
      '';

      pim.timetracking.rules =
        mkArbttProgramRule (with config.attributes.browser; [ default.windowClass fallback.windowClass ]) "activity:web"
        + "\n" + mkArbttBrowserTitleRule [ "Gmail" ] "web:email" + "\n"
        + mkArbttBrowserTitleRule [ "Google" "DuckDuckGo" ] "web:search" + "\n"
        + mkArbttBrowserTitleRule [ "wikipedia" ] "site:wikipedia";

      programs.browserpass.enable = config.browsers.firefox.enable || config.browsers.chromium.enable;
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.atomic-chrome ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/browsers.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ prefix "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch -use-fallback";
          mode = "root";
        }
        {
          key = [ prefix "Control" "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch -prompt";
          mode = "root";
        }
        {
          key = [ prefix "Control" "Shift" "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch -prompt -use-fallback";
          mode = "root";
        }
        {
          key = [ prefix "j" ];
          cmd = "${nurpkgs.toolbox}/bin/webjumps";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "j" ];
          cmd = "${nurpkgs.toolbox}/bin/webjumps -use-fallback";
          mode = "root";
        }
        {
          key = [ prefix "Control" "j" ];
          cmd = "${nurpkgs.toolbox}/bin/webjumps -copy";
          mode = "root";
        }
        {
          key = [ "c" ];
          cmd = "${nurpkgs.toolbox}/bin/links";
          mode = "browser";
        }
      ];
    })
  ];
}
