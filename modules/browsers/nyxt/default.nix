{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.browsers.nyxt;
  user = config.attributes.mainUser.name;
  prefix = config.wmCommon.prefix;
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
        description = ''
          Next should be the fallback browser.
        '';
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
        xdg.configFile."nyxt/init.lisp".text = readSubstituted ../../subst.nix ./init.lisp; # NOTE: actually absent
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
        {
          assertion = config.browsers.core.enable;
          message = "browsers/nyxt: must enable browsers/core.";
        }
      ];

      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "nyxt.desktop";
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
        {
          assertion = config.browsers.core.enable;
          message = "browsers/nyxt: must enable browsers/core.";
        }
      ];
      attributes.browser.fallback.cmd = cfg.command;
      attributes.browser.fallback.windowClass = cfg.windowClass;
    })
  ];
}
