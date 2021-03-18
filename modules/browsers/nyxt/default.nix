{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.browsers.nyxt;
  user = config.attributes.mainUser.name;
in {
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
      customConfig.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable custom configuration settings";
      };
      customConfig.path = mkOption {
        type = types.str;
        default = "nyxt/init.lisp";
        description = "Path to custom configuration file under XDG config dir";
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
      } // optionalAttrs (cfg.customConfig.enable) {
        # TODO: consider using attributes
        xdg.configFile."${cfg.customConfig.path}".text = readSubstituted ../../subst.nix ./init.lisp;
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      environment.sessionVariables = { BROWSER = cfg.command; };
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
      };
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
    })
  ];
}
