{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.browsers.nyxt;
  user = config.attributes.mainUser.name;
in
{
  options = {
    browsers.nyxt = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Next'.
        '';
      };
      downloadPath = mkOption {
        type = types.str;
        default = homePrefix "Downloads";
        description = ''
          Downloads path'.
        '';
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Next should be the default browser.
        '';
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = "Next should be the fallback browser";
      };
      extraConfig = mkOption {
        type = types.lines;
        default = "";
        description = "Extra configuration";
      };
      command = mkOption {
        type = types.str;
        default = "${pkgs.nyxt}/bin/nyxt";
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "nyxt" ];
        visible = false;
        internal = true;
        description = "Nyxt default window class.";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ nyxt ];
        xdg.configFile."nyxt/init.lisp" = {
          source = ./config/init.lisp;
          force = true;
        };
        xdg.configFile."nyxt/auto-config.lisp" = {
          source = ./config/auto-config.lisp;
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

      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "nyxt.desktop";
        home.activation.ensureNyxtIsDefault = {
          after = [ ];
          before = [ "linkGeneration" ];
          data = "${pkgs.xdg-utils}/bin/xdg-settings set default-web-browser nyxt.desktop";
        };
        programs.zsh.sessionVariables = {
          TB_DEFAULT_BROWSER = cfg.command;
        };
      };
      environment.sessionVariables.TB_DEFAULT_BROWSER = [ cfg.command ];

      attributes.browser.default.cmd = cfg.command;
      attributes.browser.default.windowClass = cfg.windowClass;
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
      attributes.browser.fallback.cmd = cfg.command;
      attributes.browser.fallback.windowClass = cfg.windowClass;

      home-manager.users.${user} = {
        programs.zsh.sessionVariables = {
          TB_FALLBACK_BROWSER = cfg.command;
        };
      };
      environment.sessionVariables.TB_FALLBACK_BROWSER = [ cfg.command ];
    })
  ];
}
