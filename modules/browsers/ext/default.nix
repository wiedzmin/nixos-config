{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.browsers.ext;
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  inherit (config.wmCommon) prefix;
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
          message = "browsers/core: must enable systemtraits maintenance.";
        }
      ];

      workstation.systemtraits.instructions = with config.navigation.bookmarks; ''
        ${pkgs.redis}/bin/redis-cli set nav/webjumps ${
          mkRedisJSON (webjumpsMeta entries separator.fields separator.tags)
        }
        ${pkgs.redis}/bin/redis-cli set nav/searchengines ${
          mkRedisJSON (searchenginesMeta entries separator.fields separator.tags)
        }
      '';

      pim.timetracking.rules =
        mkArbttProgramRule (with config.attributes.browser; [ default.traits.wmClass fallback.traits.wmClass ]) "activity:web"
        + "\n" + mkArbttBrowserTitleRule [ "Gmail" ] "web:email" config.attributes.browser + "\n"
        + mkArbttBrowserTitleRule [ "Google" "DuckDuckGo" ] "web:search" config.attributes.browser + "\n"
        + mkArbttBrowserTitleRule [ "wikipedia" ] "site:wikipedia" config.attributes.browser;

      programs.browserpass.enable = config.browsers.firefox.enable || config.browsers.chromium.enable;
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.config =
        config.browsers.ext.emacs.browseUrlSetup +
        builtins.readFile ./elisp/browsers-ext.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.modeBindings = {
        "browser" = [ prefix "b" ];
      };
      wmCommon.keybindings.entries = [
        {
          key = [ prefix "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch";
          mode = "root";
          leaveFullscreen = true;
        }
        {
          key = [ prefix "Shift" "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch -use-fallback";
          mode = "root";
          leaveFullscreen = true;
        }
        {
          key = [ prefix "Control" "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch -prompt";
          mode = "root";
          leaveFullscreen = true;
        }
        {
          key = [ prefix "Control" "Shift" "slash" ];
          cmd = "${nurpkgs.toolbox}/bin/websearch -prompt -use-fallback";
          mode = "root";
          leaveFullscreen = true;
        }
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ prefix "j" ];
          cmd = "go#webjumps";
          mode = "root";
        })
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ prefix "Shift" "j" ];
          cmd = "go#webjumps -use-fallback";
          mode = "root";
        })
        (goLocalDebugKeybinding config.dev.golang.goPath config.attributes.debug.useLocalGoBinaries {
          key = [ prefix "Control" "j" ];
          cmd = "go#webjumps -copy";
          mode = "root";
        })
        {
          key = [ "c" ];
          cmd = "${nurpkgs.toolbox}/bin/links";
          mode = "browser";
        }
      ];
    })
  ];
}
