{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

# TODO: add options for styling (colorscheme, font) and relay these settings to `appearance` submodules
# examples:
# (define-configuration prompt-buffer
#   ((style
# "* { font-family: monospace,monospace; font-size: 18px; line-height: 18px; }
# body { overflow: hidden; margin: 0; padding: 0; }
# #prompt-area { background-color: dimgray; display: grid; grid-template-columns: auto auto 1fr; width: 100%; color: white; }
# #prompt { padding-left: 10px; line-height: 26px; }
# #prompt-extra { line-height: 26px; padding-right: 7px; }
# #input { border: none; outline: none; padding: 3px; background-color: #E8E8E8; width: 100%; autofocus: true; }
# .source { margin-left: 10px; margin-top: 15px; }
# .source-glyph { margin-right: 3px; }
# .source-name { color: white; padding-left: 5px; line-height: 24px; background-color: gray; }
# #suggestions { overflow-y: hidden; overflow-x: hidden; height: 100%; width: 100%; }
# .source-content { margin-left: 10px; background-color: #F7F7F7; width: 100%; table-layout: fixed; }
# .source-content th:first-child { width: 20%; }
# .source-content th:nth-child(2) { width: 20%; }
# .source-content td { white-space: nowrap; height: 20px; overflow: auto; }
# .source-content th { font-weight: normal; padding-left: 3px; text-align: left; background-color: #E8E8E8; }
# .source-content td::-webkit-scrollbar { display: none; }
# #selection { background-color: 575757; color: white; }
# .marked { background-color: darkgray; font-weight: bold; color: white; }
# .selected { background-color: gray; color: white; }")))

# ~/.local/share/nyxt/history/default.lisp - history
# ~/.local/share/nyxt/sessions/default.lisp - sessions

# TODO: play with customizations from https://github.com/bqv/rc/blob/d1aecf3d4243fadcf9874c23909160d808072674/users/browsers/nyxt/default.nix
let
  cfg = config.browsers.nyxt;
  user = config.attributes.mainUser.name;
  standardDesktopID = "nyxt";
  customDesktopID = "org.custom.nyxt.custom";
in
{
  options = {
    browsers.nyxt = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Nyxt'.
        '';
      };
      # TODO: https://discourse.atlas.engineer/t/how-to-open-a-new-browser-window-from-command-line/811
      traits = mkOption {
        type = types.submodule (import ../../workstation/systemtraits/xapp-traits.nix);
        description = "Nyxt application traits";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Nyxt should be the default browser.
        '';
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = "Nyxt should be the fallback browser";
      };
      desktopID = mkOption {
        type = types.enum [ standardDesktopID customDesktopID ];
        default = customDesktopID;
        description = "Desktop entry name";
      };
      emacs.browseUrlSetup = mkOption {
        type = types.lines;
        default = ''
          (use-package browse-url
            :config
            (setq browse-url-browser-function 'browse-url-generic)
            (setq browse-url-generic-program "${cfg.traits.command.binary}"))
        '';
        visible = false;
        internal = true;
        description = "Specialized Nyxt-aware `browse-url` package setup";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable Nyxt-related bookmarks";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      browsers.nyxt.traits = rec {
        command = {
          binary = "${pkgs.nyxt}/bin/nyxt";
        };
        wmClass = [ "nyxt" "Nyxt" ];
      };
      home-manager.users."${user}" = {
        home.packages = with pkgs; [
          nyxt
          (makeDesktopItem {
            name = customDesktopID;
            type = "Application";
            exec = "${appCmdFull cfg.traits} %U";
            comment = "Custom Nyxt desktop item, customizations pending after more close usage";
            desktopName = "Nyxt";
            categories = [ "Network" "WebBrowser" ];
          })
          extract_url
          xurls
        ];
        xdg.configFile."nyxt/init.lisp" = {
          text = builtins.readFile ./config/init.lisp;
          force = true;
        };
        xdg.configFile."nyxt/external.lisp" = {
          text = readSubstituted config inputs pkgs [ ./subst/external.nix ] [ ./config/external.lisp ];
          force = true;
        };
        xdg.configFile."nyxt/appearance.lisp" = {
          text = builtins.readFile ./config/appearance.lisp;
          force = true;
        };
        xdg.configFile."nyxt/auto-config.lisp" = {
          text = builtins.readFile ./config/auto-config.lisp;
          force = true;
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: nyxt: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.chromium.isDefault && !config.browsers.qutebrowser.isDefault
            && !config.browsers.firefox.isDefault;
          message = "browsers: nyxt: there should be exactly one default.";
        }
      ];
      home-manager.users."${user}" = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "${cfg.desktopID}.desktop";
      };
      services.xserver.displayManager.sessionCommands = ''
        ${pkgs.xdg-utils}/bin/xdg-settings set default-web-browser ${cfg.desktopID}.desktop
      '';
      shell.core.variables = [{ TB_DEFAULT_BROWSER = appCmdFull cfg.traits; global = true; }];

      attributes.browser.default.traits = cfg.traits;

      browsers.ext.emacs.browseUrlSetup = cfg.emacs.browseUrlSetup;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: nyxt: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.chromium.isFallback && !config.browsers.qutebrowser.isFallback
            && !config.browsers.firefox.isFallback;
          message = "browsers: nyxt: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback.traits = cfg.traits;
      shell.core.variables = [{ TB_FALLBACK_BROWSER = appCmdFull cfg.traits; global = true; }];
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "nyxt" = mkGithubBookmark "atlas-engineer" "nyxt" roots // {
          tags = [ "nyxt" "repo" ];
          remote.browser = appCmdFull config.attributes.browser.default.traits;
          transient = true;
        };
        "nyxt/discourse" = {
          tags = [ "nyxt" "forum" ];
          remote = {
            url = "https://discourse.atlas.engineer/";
            browser = appCmdFull config.attributes.browser.default.traits;
          };
        };
      };
    })
  ];
}
